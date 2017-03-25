using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core
{
    public class SourceInformation
    {
        public SourceInformation(string streamName, long line, long column)
        {
            SourceName = streamName;
            Line = line;
            Column = column;
        }

        public string SourceName { get; set; }
        public long Line { get; set; }
        public long Column { get; set; }
    }
}
