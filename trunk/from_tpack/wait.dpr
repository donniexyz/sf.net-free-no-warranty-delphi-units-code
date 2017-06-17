program wait;

{$APPTYPE CONSOLE}
{$A+,B-,D-,E-,F-,G-,I+,L-,N-,O-,P-,Q-,R-,S+,T-,V+,X+}

uses
  Windows,
  Sysutils;

begin
  writeln('Waiting '+ParamStr(1)+' seconds... (hit Ctrl-C to cancel)');
  sleep(StrToIntDef(ParamStr(1),0)*1000);
end.
