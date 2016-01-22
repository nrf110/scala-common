package com.github.nrf110.io

import akka.util.{ByteStringBuilder, ByteString}

import scala.annotation.tailrec

package object protocol {
  sealed trait Result[+A]
  case class Success[A](a: A) extends Result[A]
  case class Incomplete[A](next: Decoder[A]) extends Result[A]
  case class Failure(cause: Throwable) extends Result[Nothing]

  sealed abstract class Decoder[+A] extends (ByteString => (Result[A], ByteString)) {
    /**
     * Parses the given ByteString to produce a Result[A].  Also returns any unused input.
     * @param bytes the input ByteString to be parsed
     * @return a pair-tuple containing the result and any unused input
     */
    def apply(bytes: ByteString): (Result[A], ByteString)

    /**
     * Return a new Parser that transforms the result of the current Parser
     * @param op the transformation function
     * @tparam B the type of the new Parser
     * @return the new Parser
     */
    def map[B](op: A => B): Decoder[B] = Decoder { bytes =>
      apply(bytes) match {
        case (Success(a), remaining) => (Success(op(a)), remaining)
        case (Incomplete(next), remaining) => (Incomplete(next map op), remaining)
        case (f: Failure, remaining) => (f, remaining)
      }
    }

    /**
     * Take the result of the current Parser and use it to create a new Parser.
     * @param op function that takes in the result of this parser and returns a new Parser.
     * @tparam B the type of the new Parser
     * @return the new Parser
     */
    def flatMap[B](op: A => Decoder[B]): Decoder[B] = Decoder { bytes =>
      apply(bytes) match {
        case (Success(a), remaining) => op(a)(remaining)
        case (Incomplete(next), remaining) => (Incomplete(next flatMap op), remaining)
        case (f: Failure, remaining) => (f, remaining)
      }
    }
  }

  object Decoder {
    /**
     * Syntactic sugar for creating a new anonymous Parser implementation.
     */
    def apply[A](op: ByteString => (Result[A], ByteString)): Decoder[A] =
      new Decoder[A] { def apply(bytes: ByteString): (Result[A], ByteString) = op(bytes) }

    /**
     * Syntactic sugar for finding a Decoder for type A
     * @tparam A the type object to be Decoded
     * @return the Decoder for A
     */
    def of[A : Decoder]: Decoder[A] = implicitly[Decoder[A]]

    /**
     * Turns an Option[Decoder[A]] into a Decoder[Option[A]]
     */
    def opt[A](decoder: Option[Decoder[A]]): Decoder[Option[A]] =
      decoder.fold(Decoder.success[Option[A]](None))(_.map(Some.apply))

    /**
     * Executes the given Decoder if the condition is true
     * @param condition The condition that determines if decoder should execute
     * @param decoder the Decoder to be used
     * @tparam A the type of the object to be Decoded
     * @return a Decoder that wraps the result of the decoder in Some if cond is true, or in None otherwise
     */
    def decodeIf[A](condition: => Boolean)(decoder: Decoder[A]): Decoder[Option[A]] =
      if (condition) decoder.map(Some.apply)
      else Decoder.success[Option[A]](None)

    /**
     * Returns value without consuming any input.
     * @param value the value to be returned
     * @tparam A the type of value
     * @return a Parser that returns a Success(value)
     */
    def success[A](value: A): Decoder[A] = Decoder((Success(value), _))

    /**
     * Always returns a Failure with the given cause.  Does not consume any input.
     * @param cause the Throwable that caused the failure
     * @return a Parser that returns a Failure(cause)
     */
    def failure(cause: Throwable): Decoder[Nothing] = Decoder((Failure(cause), _))

    /**
     * Look ahead at the next n bytes of the input without actually consuming the input.
     * @param n the number of bytes to peek
     * @return the first n bytes
     */
    def peek(n: Int = 1): Decoder[ByteString] = Decoder { bytes => (Success(bytes take n), bytes) }

    /**
     * Look ahead at the input to see if it has data.  Does not consume any input.
     * @return a flag indicating if the input is non-empty
     */
    def nonEmpty: Decoder[Boolean] = Decoder { bytes => (Success(bytes.nonEmpty), bytes) }

    /**
     * Consume the specified number of bytes from the input and return them.
     * @param n the number of bytes to consume and return
     * @return n consumed bytes
     */
    def take(n: Int): Decoder[ByteString] = Decoder { bytes =>
      if (bytes.length >= n) {
        val (taken, remaining) = bytes splitAt n
        (Success(taken), remaining)
      } else (Incomplete(take(n)), bytes)
    }

    /**
     * Keep consuming input until hitting delimiter.  Return all input up to delimiter, and consume delimiter so it
     * is not consumed by the next Parser.
     * @param delimiter the ByteString at which we return our input.
     * @return all input up to delimiter
     */
    def takeUntil(delimiter: ByteString): Decoder[ByteString] = Decoder { bytes =>
      val startIndex = bytes indexOfSlice delimiter
      val endIndex = startIndex + delimiter.length

      if (startIndex > 0) (Success(bytes take startIndex), bytes drop endIndex)
      else (Incomplete(takeUntil(delimiter)), bytes)
    }

    /**
     * Consumes all remaining input
     * @return all input
     */
    def takeAll: Decoder[ByteString] = Decoder { bytes => (Success(bytes), ByteString.empty) }

    /**
     * Continue running parser until one of parser's results returns true for cond.  For each successful parser result,
     * execute act.  Finally, execute onComplete.
     * @param cond the predicate to be satisfied to terminate the loop
     * @param act processing to be done on each successful result
     * @param parser the parser to be run in a loop
     * @param onComplete a function to run after cond is met.  useful for clean-up or signalling end of input.
     * @tparam A the type of item to be parsed
     * @return a Parser[Unit]
     */
    def eachUntil[A](cond: A => Boolean,
                     act: A => Unit,
                     parser: Decoder[A],
                     onComplete: () => Unit = { () => }): Decoder[Unit] = Decoder { bytes =>
      @tailrec
      def step(buffer: ByteString): (Result[Unit], ByteString) = {
        parser(buffer) match {
          case (Incomplete(_), _) => (Incomplete(eachUntil(cond, act, parser, onComplete)), buffer)
          case (f: Failure, remaining) => (f, remaining)
          case (Success(a), remaining) =>
            act(a)
            if (cond(a)) {
              onComplete()
              (Success(()), remaining)
            } else step(remaining)
        }
      }

      step(bytes)
    }
  }

  /**
   * Encodes an object of type A into a ByteString
   */
  trait Encoder[A] {
    def apply(a: A): ByteString
  }

  object Encoder {
    def apply[A](op: A => ByteString): Encoder[A] = new Encoder[A] {
      def apply(a: A): ByteString = op(a)
    }
    def empty[A] = apply[A](_ => ByteString.empty)
    def encode[A : Encoder](a: A): ByteString = {
      val encoder = implicitly[Encoder[A]]
      encoder(a)
    }
  }

  type EncoderField[A] = ByteStringBuilder => A => Unit
  object EncoderField {
    def apply[Input, Field](extractor: Input => Field)(implicit fieldEncoder: EncoderField[Field]): EncoderField[Input] =
      new EncoderField[Input] {
        def apply(builder: ByteStringBuilder): Input => Unit = { input: Input =>
          val field = extractor(input)
          fieldEncoder(builder)(field)
        }
      }
  }

  trait EncoderBuilder[A] {
    def andThen(field: EncoderField[A]): EncoderBuilder[A]
    def build(): Encoder[A]
  }

  object EncoderBuilder {
    def builder[A]: EncoderBuilder[A] = ImmutableEncoderBuilder(Vector.empty[EncoderField[A]])
  }

  case class ImmutableEncoderBuilder[A] private[protocol](fields: Vector[EncoderField[A]])
    extends EncoderBuilder[A] {
    def andThen(field: EncoderField[A]): EncoderBuilder[A] = copy[A](fields = field +: this.fields)
    def build(): Encoder[A] = Encoder[A] { a =>
      val builder = fields.foldRight(ByteString.newBuilder) { case (field, bldr) =>
        field(bldr)(a)
        bldr
      }

      builder.result()
    }
  }

  trait Implicits {
    implicit val byteStringEncoderField: EncoderField[ByteString] = {
      builder: ByteStringBuilder =>
        {
          bytes: ByteString =>
            builder ++= bytes
        }
    }

    implicit def optionEncoderField[A](implicit encoderField: EncoderField[A]): EncoderField[Option[A]] = {
      builder: ByteStringBuilder =>
        {
          a: Option[A] =>
            a.foreach(encoderField(builder))
        }
    }
  }

  object Implicits extends Implicits
}
