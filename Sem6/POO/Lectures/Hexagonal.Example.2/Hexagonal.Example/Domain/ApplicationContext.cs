﻿using Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Domain
{
    public class ApplicationContext
    {
        public ModelDbContext Context { get; set; }
    }
}
