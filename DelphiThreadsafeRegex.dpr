program DelphiThreadsafeRegex;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ThreadsafeRegex in 'ThreadsafeRegex.pas';


procedure PrintMatch(M : TRegExMatch);
begin
//  Writeln('Matchting: ' + M.Input);
//  Writeln('with     : ' + M.Source);
//  Writeln('Match    : ' + M.Value);
//
//  FOR VAR GrpNum := 0 TO M.GroupCount-1 DO BEGIN
//    VAR Name := M.GroupName(GrpNum);
//
//    IF Name <> '' THEN BEGIN
//      Name := ' (' + Name + ')';
//    END;
//
//    WriteLn(Format('  Group %d%s: "%s"', [GrpNum, Name, M.Group(GrpNum)]));
//  END;

  WriteLn(M.DebugDescription);
end;


procedure Tests1;
begin
  VAR R := TRegEx.Create('(He)|(wo)');
//  VAR R := TRegEx.Create('(He)(ll)o');
  VAR M := R.FirstMatch('Hello world Hello world');
  PrintMatch(M);
//  FreeAndNil(R);
end;

procedure Tests2;
begin
  VAR R := TRegEx.Create('\b((?<word>\w+)\s*)+(?<end>[.?!])');
  VAR M := R.FirstMatch('The the quick brown fox  fox jumps over the lazy dog dog.');
  PrintMatch(M);

end;


procedure Tests3;
begin
  // https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.getgroupnumbers?view=net-6.0

  VAR R := TRegEx.Create('\b((?<word>\w+)\s*)+(?<end>[.?!])');
  VAR M := R.FirstMatch('This is a sentence. This is a second sentence.');

  PrintMatch(M);
end;


begin
  ReportMemoryLeaksOnShutdown := TRUE;

  Tests1;

end.
