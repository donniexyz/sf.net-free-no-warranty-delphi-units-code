
  {$IFDEF MSWINDOWS}
  // units for MSWINDOWS only
  //ucPipe in '..\ucPipe.pas',
  utIPCObj in '..\utIPCObj.pas',
  tpCritSect in '..\tpCritSect.pas',
  //tpRTTI in '..\tpRTTI.pas',
  tpShareI in '..\tpShareI.pas',
  // tpStatus in '..\tpStatus.pas', // requires utThread
  {$ENDIF }

  {$IFDEF MSWINDOWS}
  //test_ucPipe in 'test_ucPipe.pas',
  test_tpCritSect in 'test_tpCritSect.pas',
  //test_tpRTTI in 'test_tpRTTI.pas',
  test_utIpcObj in 'test_utIpcObj.pas',
  test_tpShareI in 'test_tpShareI.pas',
  {$ENDIF }

// active tests

