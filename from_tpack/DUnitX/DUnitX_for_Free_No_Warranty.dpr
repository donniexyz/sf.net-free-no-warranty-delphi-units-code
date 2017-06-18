program DUnitX_for_Free_No_Warranty;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}

{ ---------------------------------------------------------------------------- }
{ *                                                                          * }
{ *  DUnitX tests for DUnitX_for_Free_No_Warranty                            * }
{ *                                                                          * }
{ *  This file is licensed under a Creative Commons Attribution 2.5 License. * }
{ *  http://creativecommons.org/licenses/by/2.5/                             * }
{ *  If you use this file, please keep this notice intact.                   * }
{ *                                                                          * }
{ *  Original author: Ann Lynnworth, HREF Tools Corp.                        * }
{ *  Copyright (c) 2005-2017 HREF Tools Corp.                                * }
{ *                                                                          * }
{ ---------------------------------------------------------------------------- }


uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$IF Defined(MSWINDOWS) and Defined(CodeSite)}
  CodeSiteLogging,
  {$IFEND }
  ZM_AnsiStringType,  // ZM_ units are part of the ZaphodsMap project
  ZM_CodeSiteInterface,
  ZM_LoggingBase,
  ZM_UTF8StringUtils,
  NativeXml,          // Full source available in ZaphodsMaps project
  ZaphodsMap,
  DUnitX.TestFramework,
{$I DUnitX_for_Free_No_Warranty_uses.inc}
  uFileIO_Helper,     // DUnitX utility code in ZaphodsMap project
  uDUnitX_Logging_Helper;

{$R *.res}

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin

  SetCodeSiteLoggingState([cslAll]);
  CSSend('starting');

  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(false);  // NOT quiet mode !
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
    begin
      CSSendException(nil, 'DPR', E);
      System.Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
end.
