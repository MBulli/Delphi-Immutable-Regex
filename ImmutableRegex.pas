UNIT ImmutableRegex;

INTERFACE

USES
  System.Classes,
  System.Math,
  System.RegularExpressionsAPI,
  System.SysUtils;


  // TODO GetEnumerator -> NextMatch
  // TODO Matches
  // TODO Split/Replace
  // TODO Unit Tests
  // TODO Timeout feature with callbacks?

{$IFNDEF MSWINDOWS}
{$Message Fatal 'ImmutableRegex currently supports only UTF16 and thus Windows.'}
{$ENDIF}

{$SCOPEDENUMS ON}

TYPE TRegExCreation = (NOP, Compile, CompileAndStudy, JitCompile, JitCompilePartialSoft, JitCompilePartialHard);

TYPE TRegExStudyOptions = (None, JitNormal, JitPartialSoft, JitPartialHard);

  // Copied from TPerlRegExOptions
TYPE TRegExOptions = set of (
  reoCaseLess,               // /i -> Case insensitive
  reoMultiLine,              // /m -> ^ and $ also match before/after a newline, not just at the beginning and the end of the string
  reoSingleLine,             // /s -> Dot matches any character, including \n (newline). Otherwise, it matches anything except \n
  reoExtended,               // /x -> Allow regex to contain extra whitespace, newlines and Perl-style comments, all of which will be filtered out
  reoAnchored,               // /A -> Successful match can only occur at the start of the subject or right after the previous match
  reoUnGreedy,               // Repeat operators (+, *, ?) are not greedy by default (i.e. they try to match the minimum number of characters instead of the maximum)
  reoNoAutoCapture,          // (group) is a non-capturing group; only named groups capture
  reoJavascriptCompat        // PCRE's behaviour is changed in some ways so that it is compatible with JavaScript rather than Perl.
);

// TODO Add None enum value for 'use build value'?!
TYPE TRegExNewLineMode = (
  Any,
  CR,
  LF,
  CRLF,
  AnyCRLF
);

{$SCOPEDENUMS OFF}

/// Compile time settings or optional features for pcre.
TYPE TPcreBuildConfig = RECORD
  // see https://www.pcre.org/original/doc/html/pcre_config.html
  PRIVATE
    FJit         : Boolean;
    FJitTarget   : STRING;
    FLinkSize    : Integer;

    FParensLimit         : Cardinal;
    FMatchLimit          : Cardinal;
    FMatchLimitRecursion : Cardinal;

    FNewLine : TRegExNewLineMode;
    FBsr     : Integer;

    FPosixMallocThreshold : Integer;
    FStackRecurse         : Integer;

    FUtf8  : Boolean;
    FUtf16 : Boolean;
    FUtf32 : Boolean;

    FUnicodeProperties : Boolean;

  PUBLIC
    /// Availability of just-in-time compiler support
    PROPERTY Jit       : Boolean READ FJit;
    /// String containing information about the target architecture for the JIT compiler,
    /// or empty if there is no JIT support
    PROPERTY JitTarget : STRING  READ FJitTarget;
    /// Internal link size: 2, 3, or 4
    PROPERTY LinkSize  : Integer READ FLinkSize;

    /// Parentheses nesting limit
    PROPERTY ParensLimit         : Cardinal READ FParensLimit;
    /// Internal resource limit
    PROPERTY MatchLimit          : Cardinal READ FMatchLimit;
    /// Internal recursion depth limit
    PROPERTY MatchLimitRecursion : Cardinal READ FMatchLimitRecursion;

    /// Value of the default newline sequence
    PROPERTY NewLine : TRegExNewLineMode READ FNewLine;
    /// Indicates what \R matches by default:
    /// 0 = all Unicode line endings;
    /// 1 = CR, LF, or CRLF only
    PROPERTY Bsr     : Integer READ FBsr;

    /// Threshold of return slots, above which malloc() is used by the POSIX API
    PROPERTY PosixMallocThreshold : Integer READ FPosixMallocThreshold;
    /// Recursion implementation (1=stack 0=heap)
    PROPERTY StackRecurse         : Integer READ FStackRecurse;

    /// Availability of UTF-8 support
    PROPERTY Utf8  : Boolean READ FUtf8;
    /// Availability of UTF-16 support
    PROPERTY Utf16 : Boolean READ FUtf16;
    /// Availability of UTF-32 support
    PROPERTY Utf32 : Boolean READ FUtf32;
    /// Availability of Unicode property support
    PROPERTY UnicodeProperties : Boolean READ FUnicodeProperties;

    CLASS FUNCTION PcreVersion : STRING; STATIC;

    FUNCTION DebugDescription : STRING;
END;

TYPE TRegExCapture = RECORD
  PRIVATE
    FName     : STRING;
    FStartPos : Integer;
    FLength   : Integer;

  PUBLIC
    PROPERTY Name   : STRING  READ FName;
    PROPERTY Start  : Integer READ FStartPos;
    PROPERTY Length : Integer READ FLength;
END;


TYPE TRegExGroup = RECORD
  PRIVATE
    FName     : STRING;
    FValue    : STRING;

    FStart    : Integer;
    FLength   : Integer;

    FSuccess  : BOOLEAN; // True if the owning TRegExMatch was successfull

  PUBLIC
    PROPERTY Success : BOOLEAN READ FSuccess;
    PROPERTY Name    : STRING  READ FName;
    PROPERTY Value   : STRING  READ FValue;

    PROPERTY Start   : Integer READ FStart;
    PROPERTY Length  : Integer READ FLength;
    FUNCTION Stop    : Integer;

END;

TYPE TRegExMatch = RECORD
  PRIVATE
    FSource   : STRING;
    FInput    : STRING;
    FCaptures : TArray<TRegExCapture>;

  PUBLIC
    FUNCTION Success    : BOOLEAN;

    PROPERTY Source  : STRING READ FSource;
    PROPERTY Input   : STRING READ FInput;

    FUNCTION Start   : Integer; // The index in the string where the match starts.
    FUNCTION Stop    : Integer; // The index in the string after the last character of the match.
    FUNCTION Length  : Integer;
    FUNCTION Value   : STRING;
    FUNCTION IsEmpty : BOOLEAN;       // TODO remove as the function is inconclusive if self.Success=FALSE

    FUNCTION GroupCount : Integer; // Returns the number of captured groups in the match.

    FUNCTION Group (      GroupIndex : Integer) : TRegExGroup;  {O(1)}               OVERLOAD;
    FUNCTION Group (CONST GroupName  : STRING ) : TRegExGroup;  {O(N)}               OVERLOAD;

    FUNCTION Groups(CONST GroupNums  : IEnumerable<Integer>) : TArray<TRegExGroup>;

    FUNCTION GroupNames : TArray<STRING>; // The names of the captured groups in the match.
    FUNCTION GroupName (GroupIndex : Integer) : STRING;

    FUNCTION DebugDescription      : STRING;
    FUNCTION DebugDescriptionBrief(MatchNum : INTEGER = 1) : STRING;
END;


//TYPE TRegExMatchCollection = RECORD
//  PRIVATE
//    FMatches : TArray<TRegExMatch>;
//
//  PUBLIC
//    FUNCTION Count : Integer;
//END;

TYPE IRegEx = INTERFACE
  ['{143FCF9B-CA8F-4BD8-8C7E-F672086DCB00}']

  FUNCTION IsCompiled : BOOLEAN;
  FUNCTION IsStudied  : BOOLEAN;
  FUNCTION IsJited    : BOOLEAN;

  PROCEDURE Compile;                               // Compiles the pattern
  PROCEDURE Study(Options : TRegExStudyOptions);   // Studies the pattern and compiles it first if required

  FUNCTION IsMatch(CONST Input : STRING) : BOOLEAN; OVERLOAD;
  FUNCTION IsMatch(CONST Input : STRING; StartPos : Integer) : BOOLEAN; OVERLOAD;

  FUNCTION FirstMatch(CONST Input : STRING) : TRegExMatch; OVERLOAD;
  FUNCTION FirstMatch(CONST Input : STRING; StartPos : Integer) : TRegExMatch; OVERLOAD;

  FUNCTION Matches(CONST Input : STRING) : TArray<TRegExMatch>; OVERLOAD;
  FUNCTION Matches(CONST Input : STRING; StartPos : Integer) : TArray<TRegExMatch>; OVERLOAD;

  FUNCTION Split(CONST Input    : STRING) : TArray<STRING>; OVERLOAD;
  FUNCTION Split(CONST Input    : STRING;
                       Count    : Integer) : TArray<STRING>; OVERLOAD;
  FUNCTION Split(CONST Input    : STRING;
                       Count    : Integer;
                       StartPos : Integer) : TArray<STRING>; OVERLOAD;
END;


TYPE TRegEx = RECORD
  PUBLIC
    CLASS FUNCTION Create(CONST Source          : STRING;
                                Options         : TRegExOptions = [];
                                NewLineMode     : TRegExNewLineMode = TRegExNewLineMode.Any;
                                CompileOnCreate : TRegExCreation = TRegExCreation.CompileAndStudy) : IRegEx; STATIC;

    CLASS FUNCTION GetPcreBuildConfig : TPcreBuildConfig; STATIC;
END;


IMPLEMENTATION


TYPE TRegExImpl = CLASS(TInterfacedObject, IRegEx)
  PRIVATE
    FSource      : STRING;
    FOptions     : TRegExOptions;
    FNewLineMode : TRegExNewLineMode;

    FPattern    : Pointer;
    FCharTable  : Pointer;
    FHints      : Pointer;

    FCRLFIsNewLine : BOOLEAN;


    FUNCTION Match(CONST Input    : STRING;
                   VAR   Output   : TRegExMatch;
                         StartPos : Integer;
                         Options  : Integer = 0) : Integer;

    FUNCTION MatchNext(CONST Input     : STRING;
                       VAR   Output    : TRegExMatch;
                             Offset    : Integer) : Integer;

  PUBLIC
    CONSTRUCTOR Create(CONST Source          : STRING;
                             Options         : TRegExOptions;
                             NewLineMode     : TRegExNewLineMode;
                             CompileOnCreate : TRegExCreation);
    DESTRUCTOR  Destroy; OVERRIDE;

    FUNCTION IsCompiled : BOOLEAN;
    FUNCTION IsStudied  : BOOLEAN;
    FUNCTION IsJited    : BOOLEAN;

    PROCEDURE Compile;
    PROCEDURE Study(Options : TRegExStudyOptions);

    FUNCTION IsMatch(CONST Input : STRING) : BOOLEAN; OVERLOAD;
    FUNCTION IsMatch(CONST Input : STRING; StartPos : Integer) : BOOLEAN; OVERLOAD;

    FUNCTION FirstMatch(CONST Input : STRING) : TRegExMatch; OVERLOAD;
    FUNCTION FirstMatch(CONST Input : STRING; StartPos : Integer) : TRegExMatch; OVERLOAD;

    FUNCTION Matches(CONST Input : STRING) : TArray<TRegExMatch>; OVERLOAD;
    FUNCTION Matches(CONST Input : STRING; StartPos : Integer) : TArray<TRegExMatch>; OVERLOAD;

    FUNCTION Split(CONST Input    : STRING) : TArray<STRING>; OVERLOAD;
    FUNCTION Split(CONST Input    : STRING;
                         Count    : Integer) : TArray<STRING>; OVERLOAD;
    FUNCTION Split(CONST Input    : STRING;
                         Count    : Integer;
                         StartPos : Integer) : TArray<STRING>; OVERLOAD;
END;


// https://www.pcre.org/original/doc/html/pcredemo.html

CONST MAX_SUBEXPRESSIONS = 99;  // Copied from System.RegularExpressionsCore

TYPE PCREString = UnicodeString;

TYPE PCREOffsetList = ARRAY[0..(MAX_SUBEXPRESSIONS+1)*3] OF Integer;
TYPE PPCREOffsetList = ^PCREOffsetList;


PROCEDURE GetSubStringPos(    Offsets      : PCREOffsetList;
                              StringCount  : Integer;
                              StringNumber : Integer;
                          VAR StartPos     : Integer;
                          VAR Length       : Integer);
BEGIN
  IF (StringNumber < 0) OR (StringNumber >= StringCount) THEN BEGIN
    RAISE EArgumentOutOfRangeException.Create('GetSubStringPos: StringNumber out of range.');
  END;

  StringNumber := StringNumber*2;
  VAR StartIdx := Offsets[StringNumber  ];  // zero-based string index
  VAR StopIdx  := Offsets[StringNumber+1];  // zero-based string index

  StartPos := StartIdx + 1; // one-based string index
  Length   := StopIdx - StartIdx;
END;


{ TRegEx }

CLASS FUNCTION TRegEx.Create(CONST Source: STRING; Options : TRegExOptions; NewLineMode : TRegExNewLineMode; CompileOnCreate : TRegExCreation) : IRegEx;
BEGIN
  Result := TRegExImpl.Create(Source, Options, NewLineMode, CompileOnCreate);
END;


CLASS FUNCTION TRegEx.GetPcreBuildConfig: TPcreBuildConfig;

  FUNCTION _IntOpt(What : Integer) : Integer; INLINE;
  BEGIN
    IF 0 <> pcre_config(What, @Result) THEN BEGIN
      Result := 0;
    END;
  END;

  FUNCTION _UIntOpt(What : Integer) : Cardinal; INLINE;
  BEGIN
    IF 0 <> pcre_config(What, @Result) THEN BEGIN
      Result := 0;
    END;
  END;

  FUNCTION _BoolOpt(What : Integer) : Boolean; INLINE;
  BEGIN
    Result := _IntOpt(What) = 1;
  END;

  FUNCTION _StrOpt(What : Integer) : STRING; INLINE;
  BEGIN
    VAR Str : PAnsiChar;

    IF 0 = pcre_config(What, @Str) THEN BEGIN
      Result := STRING(Str);
    END;
  END;

BEGIN
  Result := Default(TPcreBuildConfig);

  // Calls for Utfxx are actually wrong.
  // pcre16_config(PCRE_CONFIG_UTF8) is always false, likewise pcre32_config(PCRE_CONFIG_UTF16).
  // But System.RegularExpressionsAPI adds the pcre16_ prefix to function pcre_config(),
  // so we call three times the same function which is only for one constant true,
  // even if the compiled lib would support UTF8, UTF16 and UTF32!
  Result.FUtf8  := _BoolOpt(PCRE_CONFIG_UTF8);
  Result.FUtf16 := _BoolOpt(PCRE_CONFIG_UTF16);
  Result.FUtf32 := _BoolOpt(PCRE_CONFIG_UTF32);
  Result.FUnicodeProperties := _BoolOpt(PCRE_CONFIG_UNICODE_PROPERTIES);

  Result.FJit := _BoolOpt(PCRE_CONFIG_JIT);
  Result.FJitTarget := _StrOpt(PCRE_CONFIG_JITTARGET);

  Result.FParensLimit := _UIntOpt(PCRE_CONFIG_PARENS_LIMIT);
  Result.FMatchLimit  := _UIntOpt(PCRE_CONFIG_MATCH_LIMIT);
  Result.FMatchLimitRecursion := _UIntOpt(PCRE_CONFIG_MATCH_LIMIT_RECURSION);

  CASE _IntOpt(PCRE_CONFIG_NEWLINE) OF
       -1 : Result.FNewLine := TRegExNewLineMode.Any;
       -2 : Result.FNewLine := TRegExNewLineMode.AnyCRLF;
       13 : Result.FNewLine := TRegExNewLineMode.CR;
       10 : Result.FNewLine := TRegExNewLineMode.LF;
    $0d0a : Result.FNewLine := TRegExNewLineMode.CRLF;
  END;

  Result.FBsr := _IntOpt(PCRE_CONFIG_BSR);
  Result.FLinkSize := _IntOpt(PCRE_CONFIG_LINK_SIZE);
  Result.FStackRecurse := _IntOpt(PCRE_CONFIG_STACKRECURSE);
  Result.FPosixMallocThreshold := _IntOpt(PCRE_CONFIG_POSIX_MALLOC_THRESHOLD);
END;

{ TRegExImpl }


CONSTRUCTOR TRegExImpl.Create(CONST Source : STRING; Options : TRegExOptions; NewLineMode : TRegExNewLineMode; CompileOnCreate : TRegExCreation);
BEGIN
  FSource  := Source;
  FOptions := Options;

  CASE CompileOnCreate OF
    TRegExCreation.Compile               : Compile;
    TRegExCreation.CompileAndStudy       : Study(TRegExStudyOptions.None);
    TRegExCreation.JitCompile            : Study(TRegExStudyOptions.JitNormal);
    TRegExCreation.JitCompilePartialSoft : Study(TRegExStudyOptions.JitPartialSoft);
    TRegExCreation.JitCompilePartialHard : Study(TRegExStudyOptions.JitPartialHard);
  END;
END;


DESTRUCTOR TRegExImpl.Destroy;
BEGIN
  pcre_dispose(FPattern, FHints, FCharTable);

  INHERITED;
END;


FUNCTION TRegExImpl.IsCompiled : BOOLEAN;
BEGIN
  Result := FPattern <> NIL;
END;


FUNCTION TRegExImpl.IsJited: BOOLEAN;
BEGIN
  IF NOT IsCompiled THEN EXIT(FALSE);

  VAR Tmp := 0;
  VAR RC := pcre_fullinfo(FPattern, FHints, PCRE_INFO_JIT, @Tmp);

  Result := (RC = 0) AND (Tmp = 1);
END;


FUNCTION TRegExImpl.IsStudied: BOOLEAN;
BEGIN
  Result := FHints <> NIL;
END;


PROCEDURE TRegExImpl.Study(Options : TRegExStudyOptions);
BEGIN
  IF IsStudied THEN EXIT;
  IF NOT IsCompiled THEN BEGIN
    Compile;
  END;

  VAR PcreOptions := 0;

  CASE Options OF
    TRegExStudyOptions.None           : PcreOptions := 0;
    TRegExStudyOptions.JitNormal      : PcreOptions := PCRE_STUDY_JIT_COMPILE;
    TRegExStudyOptions.JitPartialSoft : PcreOptions := PCRE_STUDY_JIT_PARTIAL_SOFT_COMPILE;
    TRegExStudyOptions.JitPartialHard : PcreOptions := PCRE_STUDY_JIT_PARTIAL_HARD_COMPILE;
  END;

  VAR Error : MarshaledAString;

  FHints := pcre_study(FPattern, PcreOptions, @Error);

  IF Error <> NIL THEN RAISE Exception.CreateFmt('Failed to study RegEx with error "%s".', [STRING(Error)]);
END;


PROCEDURE TRegExImpl.Compile;

  CONST WhitespaceChars : TArray<CHAR> = [' ', #10, #13];

  FUNCTION _GenerateOptions : Integer;
  BEGIN
    Result := PCRE_UTF16; // System.RegularExpressionsCore uses always PCRE_UTF8 any platform, but Windows is UTF16?!

    IF reoCaseLess         IN FOptions THEN Result := Result OR PCRE_CASELESS;
    IF reoMultiLine        IN FOptions THEN Result := Result OR PCRE_MULTILINE;
    IF reoSingleLine       IN FOptions THEN Result := Result OR PCRE_DOTALL;
    IF reoExtended         IN FOptions THEN Result := Result OR PCRE_EXTENDED;
    IF reoAnchored         IN FOptions THEN Result := Result OR PCRE_ANCHORED;
    IF reoUnGreedy         IN FOptions THEN Result := Result OR PCRE_UNGREEDY;
    IF reoNoAutoCapture    IN FOptions THEN Result := Result OR PCRE_NO_AUTO_CAPTURE;
    IF reoJavascriptCompat IN FOptions THEN Result := Result OR PCRE_JAVASCRIPT_COMPAT;

    CASE FNewLineMode OF
      TRegExNewLineMode.Any     : Result := Result OR PCRE_NEWLINE_ANY;
      TRegExNewLineMode.CR      : Result := Result OR PCRE_NEWLINE_CR;
      TRegExNewLineMode.LF      : Result := Result OR PCRE_NEWLINE_LF;
      TRegExNewLineMode.CRLF    : Result := Result OR PCRE_NEWLINE_CRLF;
      TRegExNewLineMode.AnyCRLF : Result := Result OR PCRE_NEWLINE_ANYCRLF;
      ELSE                        Result := Result OR PCRE_NEWLINE_ANY;
    END;
  END;

BEGIN
  IF IsCompiled THEN EXIT;
  IF FSource = '' THEN RAISE Exception.Create('Empty RegEx!');  // TODO is a empty pattern valid? in theory yes

  VAR PcreOptions := _GenerateOptions;
  VAR Error       : MarshaledAString;
  VAR ErrorOffset : Integer;

  FCharTable := pcre_maketables;
  FPattern   := pcre_compile(PCRE_STR(PCREString(FSource)), PcreOptions, @Error, @ErrorOffset, FCharTable);

  IF FPattern = NIL THEN RAISE Exception.CreateFmt('Failed to compile RegEx with error "%s", at index %d.', [STRING(Error), ErrorOffset]);

  // TODO: Also check Build info if FNewLineMode is set to None.
  FCRLFIsNewLine := FNewLineMode IN [TRegExNewLineMode.CRLF, TRegExNewLineMode.ANY, TRegExNewLineMode.ANYCRLF]
END;


FUNCTION TRegExImpl.IsMatch(CONST Input: STRING) : BOOLEAN;
BEGIN
  VAR Tmp : TRegExMatch;
  Match(Input, Tmp, 1);
  Result := Tmp.Success;
END;


FUNCTION TRegExImpl.IsMatch(CONST Input : STRING; StartPos : Integer) : BOOLEAN;
BEGIN
  VAR Tmp : TRegExMatch;
  Match(Input, Tmp, StartPos);
  Result := Tmp.Success;
END;


FUNCTION TRegExImpl.FirstMatch(CONST Input : STRING) : TRegExMatch;
BEGIN
  Match(Input, Result, 1);
END;


FUNCTION TRegExImpl.FirstMatch(CONST Input : STRING; StartPos : Integer) : TRegExMatch;
BEGIN
  IF NOT IsCompiled THEN RAISE EInvalidOperation.Create('RegEx must be compiled before matched against the input string.');
  IF NOT InRange(StartPos, 1, Length(Input)) THEN RAISE EArgumentOutOfRangeException.Create(Format('Argument StartPos=%d exceeds Input length=%d', [StartPos, Length(Input)]));

  Match(Input, Result, StartPos);
END;


FUNCTION TRegExImpl.Matches(CONST Input : STRING) : TArray<TRegExMatch>;
BEGIN
  Result := Matches(Input, 1);
END;


FUNCTION TRegExImpl.Matches(CONST Input : STRING; StartPos : Integer) : TArray<TRegExMatch>;

  PROCEDURE _AddToResult(M : TRegExMatch);
  BEGIN
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := M;
  END;

BEGIN
  IF NOT IsCompiled THEN RAISE EInvalidOperation.Create('RegEx must be compiled before matched against the input string.');
  IF Length(Input) = 0 THEN EXIT;
  IF NOT InRange(StartPos, 1, Length(Input)) THEN RAISE EArgumentOutOfRangeException.Create('StartPos');

  VAR StartOffset := StartPos;
  VAR LastMatch   := Default(TRegExMatch);

  WHILE True DO BEGIN
    MatchNext(Input, LastMatch, StartOffset);

    IF NOT LastMatch.Success THEN BREAK;

    _AddToResult(LastMatch);
    StartOffset := LastMatch.Stop;
  END;
END;

FUNCTION TRegExImpl.MatchNext(CONST Input  : STRING;
                              VAR   Output : TRegExMatch;
                                    Offset : Integer) : Integer;
BEGIN
  VAR LastMatch := Output;
  Output := Default(TRegExMatch);

  // see PCRE_NOTEMPTY_ATSTART in http://www.pcre.org/pcre.txt
  VAR LastMatchWasNullString := False;

  // If the previous match was for an empty string, we are finished if we are
  // at the end of the subject. Otherwise, arrange to run another match at the
  // same point to see if a non-empty match can be found.
  IF LastMatch.Success AND (LastMatch.Length = 0) THEN BEGIN
    IF LastMatch.Start = Length(Input)+1 THEN BEGIN
      EXIT(0);
    END;

    LastMatchWasNullString := True;
  END;

  // Next match
  Result := Match(Input, LastMatch, Offset, IfThen(LastMatchWasNullString, PCRE_NOTEMPTY_ATSTART OR PCRE_ANCHORED, 0));

  // Empty match?
  IF Result = PCRE_ERROR_NOMATCH THEN BEGIN
    IF NOT LastMatchWasNullString THEN BEGIN
      // If the last match was not a null string and we match a null string now,
      // it's a valid match and we're done.
      EXIT;
    END;

    Inc(Offset);

    IF     FCRLFIsNewLine
       AND (Offset <= Length(Input) - 1)
       AND (Input[Offset] = #13)
       AND (Input[Offset+1] = #10) THEN
    BEGIN
      // we are at CRLF, advance by one more.
      Inc(Offset);
    END;

    // If the previous match was for an empty string, we are finished if we are
    // at the end of the subject. Otherwise, arrange to run another match at the
    // same point to see if a non-empty match can be found.
    IF LastMatch.Success AND (LastMatch.Length = 0) THEN BEGIN
      IF LastMatch.Start = Length(Input)+1 THEN BEGIN
        EXIT(0);
      END;
    END;

    // Match again after zero length match aka null string
    LastMatchWasNullString := NOT LastMatchWasNullString;

    // Next match
    Result := Match(Input, LastMatch, Offset, IfThen(LastMatchWasNullString, PCRE_NOTEMPTY_ATSTART OR PCRE_ANCHORED, 0));
  END;

  // Match succeeded
  Output := LastMatch;
END;


FUNCTION TRegExImpl.Match(CONST Input : STRING; VAR Output : TRegExMatch; StartPos, Options : Integer) : Integer;
BEGIN
  Output := Default(TRegExMatch);
  Output.FSource     := self.FSource;
  Output.FInput      := Input;

  Options := Options OR PCRE_NO_UTF8_CHECK;
  VAR Offsets : PCREOffsetList;

  Result := pcre_exec(FPattern, FHints, PCRE_STR(Input), Length(Input), StartPos-1, Options, @Offsets[0], High(Offsets));

  IF Result > 0 THEN BEGIN
    VAR OffsetCount := Result;
    SetLength(Output.FCaptures, OffsetCount);

    FOR VAR I := 0 TO OffsetCount-1 DO BEGIN
      GetSubStringPos(Offsets, OffsetCount, I, Output.FCaptures[I].FStartPos, Output.FCaptures[I].FLength);
    END;

    VAR NameCount := 0;
    pcre_fullinfo(FPattern, FHints, PCRE_INFO_NAMECOUNT, @NameCount);

    IF NameCount > 0 THEN BEGIN
      VAR NameTable     : PCRE_STR;
      VAR NameEntrySize : Integer;

      pcre_fullinfo(FPattern, FHints, PCRE_INFO_NAMETABLE, @NameTable);
      pcre_fullinfo(FPattern, FHints, PCRE_INFO_NAMEENTRYSIZE, @NameEntrySize);

      FOR VAR I := 0 TO NameCount-1 DO BEGIN
        VAR Entry := NameTable + I * NameEntrySize;

        VAR Num  := Entry[0];
        VAR Name := STRING(Entry+1);

        Output.FCaptures[Integer(Num)].FName := Name;
      END;
    END;
  END
  ELSE IF Result = PCRE_ERROR_NOMATCH THEN BEGIN
    // No error, we just have an empty Output record.
    // Actually, MatchNext() needs this return code for its logic, so just exit.
    EXIT;
  END
  ELSE IF Result < 0 THEN BEGIN
    Exception.CreateFmt('pcre_exec returned error code %d.', [Result]); // TODO custom exception type and error description
  END
  ELSE BEGIN
    Assert(Result=0); // TODO Result > 0: number of captured; Result < 0 error; Has Result = 0 a meaning?
  END;
END;



FUNCTION TRegExImpl.Split(CONST Input: STRING): TArray<STRING>;

  PROCEDURE _AddString(VAR   TheArray  : TArray<STRING>;
                       VAR   Idx       : INTEGER;
                       CONST TheString : STRING ); INLINE;
  BEGIN
    VAR L := Length(TheArray);

    IF Idx >= L THEN BEGIN
      SetLength(TheArray, GrowCollection(L, L+1));
    END;

    TheArray[Idx] := TheString;
    Inc(Idx);
  END;

BEGIN
  IF Input = '' THEN EXIT;

  VAR Matches := Self.Matches(Input);

  VAR Idx    := 0;
  VAR Offset := 1;

  FOR VAR M IN Matches DO BEGIN
    IF NOT M.Success THEN CONTINUE;

    VAR S := Copy(Input, Offset, M.Start-Offset);

    _AddString(Result, Idx, S);

    FOR VAR I := 1 TO M.GroupCount-1 DO BEGIN
      _AddString(Result, Idx, M.Group(I).Value);
    END;

    Offset := M.Stop;
  END;

  VAR SEnd := Copy(Input, Offset, Length(Input)-Offset+1);
  _AddString(Result, Idx, SEnd);

  SetLength(Result, Idx);
END;


FUNCTION TRegExImpl.Split(CONST Input: STRING; Count: Integer): TArray<STRING>;
BEGIN
  Result := Split(Input, Count, 1);
END;


FUNCTION TRegExImpl.Split(CONST Input: STRING; Count, StartPos: Integer): TArray<STRING>;
BEGIN
  IF NOT IsCompiled THEN RAISE EInvalidOperation.Create('RegEx must be compiled before matched against the input string.');
  IF NOT InRange(StartPos, 1, Length(Input)) THEN RAISE EArgumentOutOfRangeException.Create('StartPos');

  RAISE ENotImplemented.Create('Split with count is not implemented yet');
END;


{ TRegExMatch }


FUNCTION TRegExMatch.Start: Integer;
BEGIN
  IF NOT Success THEN EXIT(0);

  Result := FCaptures[0].FStartPos;
END;


FUNCTION TRegExMatch.Stop: Integer;
BEGIN
  IF NOT Success THEN EXIT(0);

  Result := FCaptures[0].FStartPos + FCaptures[0].FLength;
END;


FUNCTION TRegExMatch.Length: Integer;
BEGIN
  IF NOT Success THEN EXIT(0);

  Result := FCaptures[0].FLength
END;


FUNCTION TRegExMatch.Value: STRING;
BEGIN
  Result := Group(0).Value;
END;


FUNCTION TRegExMatch.Success: BOOLEAN;
BEGIN
  Result := GroupCount > 0;
END;


FUNCTION TRegExMatch.GroupCount: Integer;
BEGIN
  Result := System.Length(FCaptures);
END;


FUNCTION TRegExMatch.IsEmpty: BOOLEAN;
BEGIN
  IF NOT Success THEN EXIT(TRUE);

  Result := (FCaptures[0].FLength = 0);
END;


FUNCTION TRegExMatch.Group(GroupIndex : Integer) : TRegExGroup;
BEGIN
  Result := Default(TRegExGroup);

  IF NOT Success THEN EXIT;
  IF NOT InRange(GroupIndex, 0, System.Length(FCaptures)) THEN RAISE EArgumentOutOfRangeException.Create('');

  Result.FName    := FCaptures[GroupIndex].FName;
  Result.FStart   := FCaptures[GroupIndex].FStartPos;
  Result.FLength  := FCaptures[GroupIndex].FLength;
  Result.FValue   := Copy(FInput, Result.FStart, Result.FLength);
  Result.FSuccess := self.Success;
END;


FUNCTION TRegExMatch.Group (CONST GroupName : STRING ) : TRegExGroup;
BEGIN
  Result := Default(TRegExGroup);

  FOR VAR I := 0 TO System.Length(FCaptures)-1 DO BEGIN
    IF FCaptures[I].FName = GroupName THEN BEGIN
      EXIT(self.Group(I));
    END;
  END;
END;


FUNCTION TRegExMatch.Groups(CONST GroupNums : IEnumerable<Integer>) : TArray<TRegExGroup>;
BEGIN
  IF NOT Success THEN EXIT;

  VAR Count := 0;
  SetLength(Result, System.Length(FCaptures));

  FOR VAR GrpIndex IN GroupNums DO BEGIN
    Result[Count] := self.Group(GrpIndex);
    Inc(Count);
  END;

  SetLength(Result, Count);
END;


FUNCTION TRegExMatch.GroupName(GroupIndex : Integer): STRING;
BEGIN
  IF NOT Success THEN EXIT('');
  IF NOT InRange(GroupIndex, 0, System.Length(FCaptures)) THEN RAISE EArgumentOutOfRangeException.Create('');

  Result := FCaptures[GroupIndex].FName;
END;


FUNCTION TRegExMatch.GroupNames : TArray<STRING>;
BEGIN
  IF NOT Success THEN EXIT;

  SetLength(Result, System.Length(FCaptures));

  FOR VAR I := 0 TO System.Length(FCaptures)-1 DO BEGIN
    Result[I] := FCaptures[I].FName;
  END;
END;



FUNCTION TRegExMatch.DebugDescription: STRING;
BEGIN
  Result :=          'Matchting: ' + self.Input  + sLineBreak;
  Result := Result + 'with     : ' + self.Source + sLineBreak;
  Result := Result + 'Match    : ' + self.Value  + sLineBreak;

  FOR VAR GrpNum := 0 TO self.GroupCount-1 DO BEGIN
    VAR Name := self.GroupName(GrpNum);

    IF Name <> '' THEN BEGIN
      Name := ' (' + Name + ')';
    END;

    Result := Result + Format('  Group %d%s: %d-%d "%s"',
                                 [GrpNum,
                                  Name,
                                  Group(GrpNum).Start,
                                  Group(GrpNum).Stop,
                                  Group(GrpNum).Value])
             + sLineBreak;
  END;

  Result := Trim(Result);
END;


FUNCTION TRegExMatch.DebugDescriptionBrief(MatchNum : INTEGER) : STRING;
BEGIN
  FOR VAR GrpNum := 0 TO GroupCount-1 DO BEGIN
    VAR Name := '';

    IF GrpNum = 0 THEN BEGIN
      Name := Format('Match %d', [MatchNum]);
    END
    ELSE IF GroupName(GrpNum) <> '' THEN BEGIN
      Name := Format('Group %s', [GroupName(GrpNum)]);
    END
    ELSE BEGIN
      Name := Format('Group %d', [GrpNum]);
    END;

    VAR Line := Format('%s %d-%d : "%s"',
                      [Name,
                       Group(GrpNum).Start,
                       Group(GrpNum).Stop,
                       Group(GrpNum).Value]);

    Result := Result + Line + sLineBreak;
  END;

  Result := Trim(Result);
END;

{ TRegExGroup }

FUNCTION TRegExGroup.Stop: Integer;
BEGIN
  Result := FStart + FLength;
END;

{ TPcreBuildConfig }

CLASS FUNCTION TPcreBuildConfig.PcreVersion: STRING;
BEGIN
  Result := STRING(System.RegularExpressionsAPI.pcre_version);
END;


FUNCTION TPcreBuildConfig.DebugDescription: STRING;

  FUNCTION _BoolToStr(ABool : BOOLEAN) : STRING; INLINE;
  BEGIN
    IF ABool
    THEN Result := 'yes'
    ELSE Result := 'no';
  END;

BEGIN
  Result :=          'Pcre version: ' + PcreVersion  + sLineBreak;
  Result := Result + ' JIT enabled: ' + _BoolToStr(Jit) + sLineBreak;
  Result := Result + ' JIT target: ' + JitTarget  + sLineBreak;
  Result := Result + ' Internal link size: ' + LinkSize.ToString  + sLineBreak;

  Result := Result + ' Limits:' + sLineBreak;
  Result := Result + ' - Parentheses nesting: ' + ParensLimit.ToString + sLineBreak;
  Result := Result + ' - Internal resource: ' + MatchLimit.ToString + sLineBreak;
  Result := Result + ' - Internal recursion depth: ' + MatchLimit.ToString + sLineBreak;

  Result := Result + ' Default newline sequence: ';
  CASE NewLine OF
    TRegExNewLineMode.Any     : Result := Result + 'Any';
    TRegExNewLineMode.CR      : Result := Result + 'CR ($0D)';
    TRegExNewLineMode.LF      : Result := Result + 'LF ($0A)';
    TRegExNewLineMode.CRLF    : Result := Result + 'CRLF ($0D$0A)';
    TRegExNewLineMode.AnyCRLF : Result := Result + 'Any CRLF';
  END;
  Result := Result + sLineBreak;

  Result := Result + ' \R matches by default: ';
  CASE Bsr OF
    0    : Result := Result + 'all Unicode line endings';
    1    : Result := Result + 'CR, LF, or CRLF only';
    ELSE   Result := Result + 'INVALID VALUE ' + Bsr.ToString;
  END;
  Result := Result + sLineBreak;

  Result := Result + ' PosixMallocThreshold: ' + PosixMallocThreshold.ToString + sLineBreak;
  Result := Result + ' Recursion implementation: ';
  CASE StackRecurse OF
    0    : Result := Result + 'stack';
    1    : Result := Result + 'heap';
    ELSE   Result := Result + 'INVALID VALUE ' + Bsr.ToString;
  END;
  Result := Result + sLineBreak;

  Result := Result + ' Unicode support:' + sLineBreak;
  Result := Result + ' - UTF-8: '  + _BoolToStr(Utf8) + sLineBreak;
  Result := Result + ' - UTF-16: ' + _BoolToStr(Utf16) + sLineBreak;
  Result := Result + ' - UTF-32: ' + _BoolToStr(Utf32) + sLineBreak;
  Result := Result + ' - Unicode properties: ' + _BoolToStr(UnicodeProperties) + sLineBreak;
END;



END.
