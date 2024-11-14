using System;
using System.Runtime;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using PChecker.Runtime;
using PChecker.Runtime.Values;
using PChecker.Runtime.Exceptions;
using PChecker.Runtime.StateMachines;
using System.Threading;
using System.Threading.Tasks;

namespace PImplementation
{
  public static partial class GlobalFunctions
  {
      public static Random rnd = new Random();
      public static void RandomWait(StateMachine machine)
      {
          System.Threading.Thread.Sleep(rnd.Next(10, 20));
          return;
      }
      public static void Wait(StateMachine machine)
      {
          System.Threading.Thread.Sleep(10);
          return;
      }
  }
}
