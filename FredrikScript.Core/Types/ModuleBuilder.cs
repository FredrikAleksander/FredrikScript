using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class ModuleBuilder
    {
        public ModuleBuilder(string name)
        {
            Name = name;
        }

        public string Name { get; }
    }
}
