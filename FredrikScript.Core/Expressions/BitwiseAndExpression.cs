﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FredrikScript.Core.Types;

namespace FredrikScript.Core.Expressions
{
    public class BitwiseAndExpression : BinaryExpression
    {
        public BitwiseAndExpression(Context context, SourceInformation sourceInformation, Expression leftValue, Expression rightValue) : base(context, sourceInformation, leftValue, rightValue)
        {
        }

        public override IType ExpressionType => LeftValue.ExpressionType;
    }
}
