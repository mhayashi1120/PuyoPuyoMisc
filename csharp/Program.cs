using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace PuyoPuyo
{
    class Program
    {
        static void Main(string[] args)
        {
            PuyoPuyo p = new PuyoPuyo(args[0]);

            p.Print();

            p.Boot();
        }
    }
}
