using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.ServiceProcess;
using System.IO;
using System.Threading;

namespace Zad5
{
    public partial class RunningAppsMonitor : ServiceBase
    {
        private Timer timer;


        public RunningAppsMonitor()
        {
            InitializeComponent();
            this.ServiceName = "RunningAppsMonitorService";
        }

        protected override void OnStart(string[] args)
        {
            // add logging action to timer
            timer = new Timer(LogApps, null, TimeSpan.Zero, TimeSpan.FromMinutes(1));
        }

        protected override void OnStop()
        {
            // stop timer
            timer?.Change(Timeout.Infinite, 0);
        }

        private void LogApps(object state)
        {
            try
            {
                var runningProcesses = Process.GetProcesses();
                string filePath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "RunningApplications.txt");

                using (StreamWriter writer = new StreamWriter(filePath, false))
                {
                    writer.WriteLine($"Running applications({DateTime.Now}):");

                    foreach (var process in runningProcesses)
                    {
                        writer.WriteLine($"{process.ProcessName} - {process.MainWindowTitle}");
                    }
                }
            }
            catch (Exception ex)
            {
                EventLog.WriteEntry("RunningAppsMonitorService", $"Error: {ex.Message}", EventLogEntryType.Error);
            }
        }
    }
}

/*
How to install:
run Developer Command Prompt for VS2022
go to the .exe directory
use installutil Service.exe to install

*/