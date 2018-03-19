
  ucPipe in '..\from_tpack\ucPipe.pas',

  {$IFDEF MSWINDOWS}
  // units for MSWINDOWS only
  ucLogFileAnsi in '..\from_tpack\ucLogFileAnsi.pas',
  utIPCObj in '..\from_tpack\utIPCObj.pas',

  tpCritSect in '..\from_tpack\tpCritSect.pas',
  tpRTTI in '..\from_tpack\tpRTTI.pas',
  tpShareI in '..\from_tpack\tpShareI.pas',
  //tpStatus in '..\from_tpack\tpStatus.pas', // requires utThread
  {$ENDIF }

  test_ucPipe in 'test_ucPipe.pas',

  {$IFDEF MSWINDOWS}
  test_tpCritSect in 'from_tpack\test_tpCritSect.pas',
  test_tpRTTI in 'from_tpack\test_tpRTTI.pas',
  test_utIpcObj in 'from_tpack\test_utIpcObj.pas',
  test_tpShareI in 'from_tpack\test_tpShareI.pas',
  test_ucLogFileAnsi in 'from_tpack\test_ucLogFileAnsi.pas',
  {$ENDIF }

// active tests

