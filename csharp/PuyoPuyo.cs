using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace PuyoPuyo
{
    class PuyoPuyo
    {
        private class Point
        {
            public readonly int X = -1;
            public readonly int Y = -1;

            public static readonly Point Null = new Point();

            private Point() { }

            public Point(int x, int y)
            {
                this.X = x;
                this.Y = y;
            }

            public override bool Equals(object obj)
            {
                if (obj is Point)
                {
                    var p = (Point)obj;

                    return p.X == this.X && p.Y == this.Y;
                }

                return false;
            }

            public override int GetHashCode()
            {
                return this.X;
            }
        }

        private enum PuyoColor
        {
            Green,
            Yellow,
            Red,
            Null,
        }

        private List<List<PuyoColor>> AllBlocks;

        public PuyoPuyo(string file)
        {
            this.InitializeAlist();

            this.AllBlocks = this.ReadFileBlocks(file);
        }

        public void Print()
        {
            foreach (List<PuyoColor> row in this.AllBlocks)
            {
                foreach (PuyoColor c in row)
                {
                    Console.Write(this.ColorToChar(c));
                }
                Console.WriteLine();
            }

            Console.WriteLine(new string('-', 6));
        }

        public void Boot()
        {
            List<List<Point>> groups;

            do
            {
                groups = this.CollectGroups();

                if (groups.Count == 0)
                    break;

                this.Spell(groups);

                this.Print();

            } while (true);
        }

        private void Spell(List<List<Point>> groups)
        {
            foreach (List<Point> block in groups)
            {
                foreach (Point p in block)
                {
                    this.SetColor(p, PuyoColor.Null);
                }
            }

            this.Fall();
        }

        private void Fall()
        {
            for (int y = this.AllBlocks.Count - 1; y >= 0; y--)
            {
                for (int x = 0; true; x++)
                {
                    var p = new Point(x, y);

                    if (!this.IsRangeValid(p))
                        break;

                    var c = this.GetColor(p);

                    if (c != PuyoColor.Null)
                        continue;

                    var np = this.FallingPoint(p);

                    if (np == Point.Null)
                        continue;

                    this.SetColor(p, this.GetColor(np));

                    this.SetColor(np, PuyoColor.Null);
                }
            }
        }

        private Point FallingPoint(Point p)
        {
            for (int y = p.Y - 1; y >= 0; y--)
            {
                var np = new Point(p.X, y);
                var c = this.GetColor(np);

                if (c != PuyoColor.Null)
                    return np;
            }

            return Point.Null;
        }

        private List<List<Point>> CollectGroups()
        {
            List<Point> done = new List<Point>();
            var res = new List<List<Point>>();

            int y = this.AllBlocks.Count - 1;

            foreach (List<PuyoColor> row in this.AllBlocks)
            {
                int x = 0;
                foreach (PuyoColor c in row)
                {
                    var p = new Point(x, y);

                    done.Add(p);

                    var group = this.CollectGroup(done, p);

                    if (group.Count >= 4)
                        res.Add(group);

                    x++;
                }

                y--;
            }


            return res;
        }

        private List<Point> CollectGroup(List<Point> done, Point p)
        {
            var res = new List<Point>();

            PuyoColor c = this.GetColor(p);

            if (c == PuyoColor.Null)
                return res;

            res.Add(p);

            foreach (Point p2 in new Point[] { 
                new Point(p.X + 1, p.Y), 
                new Point(p.X, p.Y + 1), 
                new Point(p.X, p.Y - 1), 
                new Point(p.X - 1, p.Y) })
            {
                if (!this.IsRangeValid(p2))
                    continue;

                if (done.Contains(p2))
                    continue;

                if (c == this.GetColor(p2))
                {
                    done.Add(p2);

                    res.AddRange(this.CollectGroup(done, p2));
                }
            }

            return res;
        }

        private bool IsRangeValid(Point p)
        {
            if (p.X < 0) return false;
            if (p.Y < 0) return false;
            if (p.X > 5) return false;
            if (p.Y >= this.AllBlocks.Count) return false;

            return true;
        }

        private void SetColor(Point p, PuyoColor color)
        {
            this.SetColor(p.X, p.Y, color);
        }

        private void SetColor(int x, int y, PuyoColor color)
        {
            this.AllBlocks[y][x] = color;
        }

        private PuyoColor GetColor(Point p)
        {
            return this.GetColor(p.X, p.Y);
        }

        private PuyoColor GetColor(int x, int y)
        {
            return this.AllBlocks[y][x];
        }

        private List<List<PuyoColor>> ReadFileBlocks(string file)
        {
            return File.ReadAllLines(file).Select(s => s.ToCharArray().Select(c => this.CharToColor(c)).ToList()).ToList();
        }

        private char ColorToChar(PuyoColor c)
        {
            return this._Alist[c];
        }

        Dictionary<PuyoColor, char> _Alist = new Dictionary<PuyoColor, char>();

        private void InitializeAlist()
        {
            _Alist.Add(PuyoColor.Null, ' ');
            _Alist.Add(PuyoColor.Green, 'G');
            _Alist.Add(PuyoColor.Yellow, 'Y');
            _Alist.Add(PuyoColor.Red, 'R');
        }

        private PuyoColor CharToColor(char c)
        {
            return this._Alist.Where(kv => kv.Value == c).Select(kv => kv.Key).First();
        }
    }
}
