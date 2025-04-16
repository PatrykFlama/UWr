using System;
using System.IO;


namespace Zad2
{
    public class CaesarStream : Stream
    {
        private readonly Stream _baseStream;
        private readonly int _shift;
        private readonly bool _leaveOpen;

        public CaesarStream(Stream baseStream, int shift, bool leaveOpen = false)
        {
            _baseStream = baseStream ?? throw new ArgumentNullException(nameof(baseStream));
            _shift = shift;
            _leaveOpen = leaveOpen;
        }

        public override bool CanRead => _baseStream.CanRead;
        public override bool CanSeek => false;
        public override bool CanWrite => _baseStream.CanWrite;
        public override long Length => throw new NotSupportedException();
        public override long Position
        {
            get => _baseStream.Position;
            set => throw new NotSupportedException();
        }

        public override void Flush() => _baseStream.Flush();

        public override int Read(byte[] buffer, int offset, int count)
        {
            int readCount = _baseStream.Read(buffer, offset, count);
            for(int i = offset; i < offset + readCount; i++)
            {
                buffer[i] = (byte)(buffer[i] + _shift);
            }
            return readCount;
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            byte[] shifted = new byte[count];
            for(int i = 0; i < count; i++)
            {
                shifted[i] = (byte)(buffer[offset + i] + _shift);
            }
            _baseStream.Write(shifted, 0, count);
        }

        public override long Seek(long offset, SeekOrigin origin) =>
            throw new NotSupportedException();

        public override void SetLength(long value) =>
            _baseStream.SetLength(value);

        protected override void Dispose(bool disposing)
        {
            if(disposing && !_leaveOpen)
                _baseStream.Dispose();

            base.Dispose(disposing);
        }
    }

    //public class Program
    //{
    //    static void Main(string[] args) { }
    //}
}
