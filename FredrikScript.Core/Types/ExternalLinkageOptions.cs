using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class ExternalLinkageOptions
    {
        private ExternalLinkageOptions()
        {

        }

        public static ExternalLinkageOptions Parse(string externString)
        {
            if (externString == null) throw new ArgumentNullException(nameof(externString));
            return new ExternalLinkageOptions();
        }
    }
}
