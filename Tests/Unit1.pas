UNIT Unit1;

INTERFACE

USES
  DUnitX.TestFramework,
  System.RegularExpressions,
  ImmutableRegex;

TYPE
  [TestFixture]
  TImmutableRegExTest = CLASS
  PUBLIC
    // Sample Methods
    // Simple single Test
    [Test]
    PROCEDURE Test1;

    [Test]
    PROCEDURE Test2;

    [Test]
    PROCEDURE TestIgnoreWhitespacesInPattern();

    [Test]
    PROCEDURE TestMatchAll;

    [Test]
    PROCEDURE TestMatchAll2;

    [Test]
    PROCEDURE TestMatchAll3;

    [Test]
    PROCEDURE TestStartOffset;

    [Test]
    PROCEDURE TestZeroLengthMatches1;

    [Test]
    PROCEDURE TestZeroLengthMatches3;

    [Test]
    PROCEDURE TestZeroLengthMatches;

    [Test]
    PROCEDURE TestZeroLengthMatches2;

    [Test]
    PROCEDURE TestEmptyMatchGroup;

    [Test]
    PROCEDURE TestMatchesEmptyInput;
  END;

IMPLEMENTATION

TYPE TGroupAsserter = RECORD
  STRICT PRIVATE
    FTheGroup : TRegExGroup;

  PUBLIC
    CONSTRUCTOR Create(AMatch : TRegExGroup);

    FUNCTION AssertIsSuccess  (SuccessExpected : BOOLEAN = TRUE) : TGroupAsserter;
    FUNCTION AssertHasValue   (CONST ValueExpected : STRING) : TGroupAsserter;
    FUNCTION AssertStartLength(StartExpected, LengthExpected : Integer) : TGroupAsserter;

    FUNCTION AssertIsZeroLengthAt(      StartExpected : Integer) : TGroupAsserter;
    FUNCTION AssertMatchAt       (      StartExpected : Integer;
                                  CONST ValueExpected : STRING) : TGroupAsserter;
END;


TYPE TMatchAsserter = RECORD
  STRICT PRIVATE
    FTheMatch : TRegExMatch;
  PUBLIC
    CONSTRUCTOR Create(AMatch : TRegExMatch);

    FUNCTION AssertIsSuccess  (SuccessExpected : BOOLEAN = TRUE) : TMatchAsserter;
    FUNCTION AssertIsEmpty    (EmptyExpected   : BOOLEAN = TRUE) : TMatchAsserter;
    FUNCTION AssertHasValue   (CONST ValueExpected : STRING) : TMatchAsserter;
    FUNCTION AssertStartLength(StartExpected, LengthExpected : Integer) : TMatchAsserter;

    FUNCTION AssertIsZeroLengthAt(      StartExpected : Integer) : TMatchAsserter;
    FUNCTION AssertMatchAt       (      StartExpected : Integer;
                                  CONST ValueExpected : STRING) : TMatchAsserter;

    FUNCTION AssertHasGroup(      GroupIndex : Integer) : TGroupAsserter;  OVERLOAD;
    FUNCTION AssertHasGroup(CONST GroupName  : STRING ) : TGroupAsserter;  OVERLOAD;

END;



PROCEDURE CompareAllMatches(CONST Pattern : STRING;
                            CONST Input   : STRING);
BEGIN
  VAR RI := TRegEx.Create(Pattern);
  VAR RD := System.RegularExpressions.TRegEx.Create(Pattern);

  VAR MI := RI.Matches(Input);
  VAR MD := RD.Matches(Input);

  Assert.AreEqual(MD.Count, Length(MI));

  FOR VAR I := 0 TO MD.Count-1 DO BEGIN

    VAR A := MD[I];
    VAR B := MI[I];

    Assert.AreEqual(A.Index,          B.Start);
    Assert.AreEqual(A.Length,         B.Length);
    Assert.AreEqual(A.Index+A.Length, B.Stop);
    Assert.AreEqual(A.Success,        B.Success);
    Assert.AreEqual(A.Value,          B.Value);

    Assert.AreEqual(A.Groups.Count, B.GroupCount);

    FOR VAR J := 0 TO A.Groups.Count-1 DO BEGIN
      VAR GA := A.Groups[J];
      VAR GB := B.Group(J);

      Assert.AreEqual(GA.Index,          GB.Start);
      Assert.AreEqual(GA.Length,         GB.Length);
      Assert.AreEqual(GA.Index+A.Length, GB.Stop);
      Assert.AreEqual(GA.Success,        GB.Success);
      Assert.AreEqual(GA.Value,          GB.Value);
    END;
  END;
END;


PROCEDURE TImmutableRegExTest.Test1;
BEGIN
  WriteLn(TRegEx.GetPcreBuildConfig.DebugDescription);

  VAR R := TRegEx.Create('\b((?<word>\w+)\s*)+(?<end>[.?!])');
  VAR M := R.FirstMatch('This is a sentence. This is a second sentence.');

  Writeln(M.DebugDescriptionBrief);

  Assert.IsTrue(M.Success);
  Assert.AreEqual(Length('This is a sentence.'), M.Length);
  Assert.AreEqual('This is a sentence.', M.Value);
  Assert.AreEqual(4, M.GroupCount);

  Assert.AreEqual('This is a sentence.', M.Group(0).Value);
  Assert.AreEqual('sentence', M.Group(1).Value);
  Assert.AreEqual('sentence', M.Group(2).Value);
  Assert.AreEqual('.', M.Group(3).Value);

  Assert.AreEqual('sentence', M.Group('word').Value);
  Assert.AreEqual('.', M.Group('end').Value);
END;


PROCEDURE TImmutableRegExTest.Test2;
BEGIN
  VAR ThePattern := 'hello';
  VAR TheInput   := 'hello';

  VAR R1 := TRegEx.Create(ThePattern);
  VAR M1 := R1.FirstMatch(TheInput);

  TMatchAsserter.Create(M1)
  .AssertIsSuccess
  .AssertHasValue('hello')
  .AssertStartLength(1, 5);

  VAR M2 := System.RegularExpressions.TRegEx.Match(TheInput, ThePattern);

  Assert.IsTrue(M2.Success);
  Assert.AreEqual('hello', M2.Value);
  Assert.AreEqual(1, M2.Index);
  Assert.AreEqual(5, M2.Length);

END;


PROCEDURE TImmutableRegExTest.TestIgnoreWhitespacesInPattern;
BEGIN
  VAR R := TRegEx.Create(' ab    c '#10#13'd  ', [reoExtended]);
  VAR M := R.FirstMatch('xyz abcd pokj');

  Assert.IsTrue(M.Success);
  Assert.AreEqual('abcd', M.Value);
END;


PROCEDURE TImmutableRegExTest.TestMatchAll;
BEGIN
//  VAR R := TRegEx.Create('hello');
//  VAR M := R.Matches('hello hello hello hello');
//  VAR M := R.Matches('hello hello hello hello');

  // .* - Match 1: 0-10 helloworld; Match 2: 10-10 null
  CompareAllMatches('hello', 'hello hello hello hello');
END;


PROCEDURE TImmutableRegExTest.TestMatchAll2;
BEGIN
  VAR R := TRegEx.Create('.*');
  VAR M := R.Matches('hello');

  WriteLn(M[0].DebugDescriptionBrief(1));
  WriteLn(M[1].DebugDescriptionBrief(2));

  // .* - Match 1: 0-5 hello; Match 2: 5-5 null
  Assert.AreEqual(2, Length(M));

  TMatchAsserter.Create(M[0])
  .AssertIsSuccess
  .AssertHasValue('hello')
  .AssertStartLength(1, 5);

  TMatchAsserter.Create(M[1])
  .AssertIsSuccess
  .AssertIsEmpty
  .AssertHasValue('')
  .AssertStartLength(6, 0);


  VAR M2 := System.RegularExpressions.TRegEx.Matches('hello', '.*');

  Assert.IsTrue(M2[0].Success);
  Assert.AreEqual('hello', M2[0].Value);
  Assert.AreEqual(1, M2[0].Index);
  Assert.AreEqual(5, M2[0].Length);

END;

PROCEDURE TImmutableRegExTest.TestMatchAll3;
BEGIN
  VAR R := TRegEx.Create('hello');
  VAR M := R.Matches('hello');
END;


PROCEDURE TImmutableRegExTest.TestStartOffset;
BEGIN
  VAR R := TRegEx.Create('.*');
  VAR M := R.FirstMatch('helloworld', 6);

  Assert.IsTrue(M.Success);
  Assert.AreEqual('world', M.Value);
END;

PROCEDURE TImmutableRegExTest.TestZeroLengthMatches1;
BEGIN
  // see https://www.regular-expressions.info/zerolength.html

  // Delphi actually returns a empty list in these cases ...
  // We stick to the behavior of PCRE (and .Net)

  // Match 1 0-0 null
  // Match 2 1-1 null
  VAR R := TRegEx.Create('\d*');

  VAR Matches := R.Matches('a');
  Assert.AreEqual(2, Length(Matches));

  TMatchAsserter.Create(Matches[0]).AssertIsZeroLengthAt(1);
  TMatchAsserter.Create(Matches[1]).AssertIsZeroLengthAt(2);
END;


PROCEDURE TImmutableRegExTest.TestZeroLengthMatches3;
BEGIN
  VAR R := TRegEx.Create('\d*');
  VAR Matches := R.Matches('1a');
  Assert.AreEqual(3, Length(Matches));

  TMatchAsserter.Create(Matches[0]).AssertHasValue('1');
  TMatchAsserter.Create(Matches[1]).AssertIsZeroLengthAt(2);
  TMatchAsserter.Create(Matches[2]).AssertIsZeroLengthAt(3);
END;

PROCEDURE TImmutableRegExTest.TestZeroLengthMatches;
BEGIN
  // see https://www.regular-expressions.info/zerolength.html

  // Delphi actually returns a empty list in these cases ...
  // We stick to the behavior of PCRE (and .Net)

  // Match 1 0-0 null
  // Match 2 1-1 null
  // Match 3 2-2 null
  // Match 4 3-3 null
  VAR R := TRegEx.Create('\d*');

  VAR Matches := R.Matches('abc');
  Assert.AreEqual(4, Length(Matches));

  FOR VAR I := 0 TO Length(Matches)-1 DO BEGIN
    TMatchAsserter.Create(Matches[I])
    .AssertIsZeroLengthAt(I+1);
  END;
END;


PROCEDURE TImmutableRegExTest.TestZeroLengthMatches2;
BEGIN
  // Delphi returns [] see TestZeroLengthMatches
  // Match 1 0-0 null
  // Match 2 0-1 x
  // Match 3 1-2 1
  // Match 4 2-2 null
  VAR R := TRegEx.Create('\d*|x');

  VAR Matches := R.Matches('x1');
  Assert.AreEqual(4, Length(Matches));

  TMatchAsserter.Create(Matches[0]).AssertIsZeroLengthAt(1);
  TMatchAsserter.Create(Matches[1]).AssertMatchAt(1, 'x');
  TMatchAsserter.Create(Matches[2]).AssertMatchAt(2, '1');
  TMatchAsserter.Create(Matches[3]).AssertIsZeroLengthAt(3);
END;


PROCEDURE TImmutableRegExTest.TestMatchesEmptyInput;
BEGIN
  VAR R := TRegEx.Create('a');

  VAR Matches := R.Matches('');
  Assert.AreEqual(0, Length(Matches));
END;


PROCEDURE TImmutableRegExTest.TestEmptyMatchGroup;
BEGIN
  // Delphi returns [] see TestZeroLengthMatches
  // Match 1 0-0 null
  //  Group 1 0-0 null
  // Match 2 1-1 null
  //  Group 1 1-1 null
  // Match 3 2-2 null
  //  Group 1 2-2 null
  VAR R := TRegEx.Create('()');

  VAR Matches := R.Matches('x1');
  Assert.AreEqual(3, Length(Matches));

  Assert.AreEqual(2, Matches[0].GroupCount);
  Assert.AreEqual(2, Matches[1].GroupCount);
  Assert.AreEqual(2, Matches[2].GroupCount);

  WriteLn(R.FirstMatch('abc').DebugDescription);

  TMatchAsserter.Create(Matches[0]).AssertIsZeroLengthAt(1).AssertHasGroup(1).AssertIsZeroLengthAt(1);
  TMatchAsserter.Create(Matches[1]).AssertIsZeroLengthAt(2).AssertHasGroup(1).AssertIsZeroLengthAt(2);
  TMatchAsserter.Create(Matches[2]).AssertIsZeroLengthAt(3).AssertHasGroup(1).AssertIsZeroLengthAt(3);
END;

// <(.*?)>
// This is <something> <something else> <something further> no more
// Match 1 8-19  <something>
// Group 1 9-18  something
// Match 2 20-36 <something else>
// Group 1 21-35 something else
// Match 3 37-56 <something further>
// Group 1 38-55 something further

// .*?
// abc
// Match 1 0-0 null
// Match 1 0-1 a
// Match 1 1-1 null
// Match 1 1-2 b
// Match 1 2-2 null
// Match 1 2-3 c
// Match 1 3-3 null


{ TMatchAsserter }

CONSTRUCTOR TMatchAsserter.Create(AMatch: TRegExMatch);
BEGIN
  FTheMatch := AMatch;
END;


FUNCTION TMatchAsserter.AssertHasGroup(GroupIndex: Integer): TGroupAsserter;
BEGIN
  // TODO Add actual assert logic like range check?
  VAR Grp := FTheMatch.Group(GroupIndex);
  Result := TGroupAsserter.Create(Grp);
END;


FUNCTION TMatchAsserter.AssertHasGroup(CONST GroupName: STRING): TGroupAsserter;
BEGIN
  // TODO Add actual assert logic like range check?
  VAR Grp := FTheMatch.Group(GroupName);
  Result := TGroupAsserter.Create(Grp);
END;


FUNCTION TMatchAsserter.AssertHasValue(CONST ValueExpected: STRING) : TMatchAsserter;
BEGIN
  Assert.AreEqual(ValueExpected, FTheMatch.Value, 'for Match.Value');

  Result := self;
END;


FUNCTION TMatchAsserter.AssertIsEmpty(EmptyExpected   : BOOLEAN = TRUE): TMatchAsserter;
BEGIN
  Assert.AreEqual(EmptyExpected, FTheMatch.IsEmpty, 'for Match.IsEmpty');

  Result := self;
END;


FUNCTION TMatchAsserter.AssertIsSuccess(SuccessExpected : BOOLEAN = TRUE) : TMatchAsserter;
BEGIN
  Assert.AreEqual(SuccessExpected, FTheMatch.Success, 'for Match.Success');

  Result := self;
END;


FUNCTION TMatchAsserter.AssertIsZeroLengthAt(StartExpected : Integer) : TMatchAsserter;
BEGIN
  self.AssertIsSuccess;
  self.AssertStartLength(StartExpected, 0);
  self.AssertHasValue('');

  Result := Self;
END;


FUNCTION TMatchAsserter.AssertMatchAt(StartExpected: Integer; CONST ValueExpected: STRING): TMatchAsserter;
BEGIN
  self.AssertIsSuccess;
  self.AssertStartLength(StartExpected, Length(ValueExpected));
  self.AssertHasValue(ValueExpected);

  Result := Self;
END;


FUNCTION TMatchAsserter.AssertStartLength(StartExpected, LengthExpected : Integer) : TMatchAsserter;
BEGIN
  VAR StopExpected := StartExpected + LengthExpected;

  Assert.AreEqual(StartExpected , FTheMatch.Start , 'for Match.Start');
  Assert.AreEqual(StopExpected  , FTheMatch.Stop  , 'for Match.Stop');
  Assert.AreEqual(LengthExpected, FTheMatch.Length, 'for Match.Length');

  Result := self;
END;




{ TGroupAsserter }

CONSTRUCTOR TGroupAsserter.Create(AMatch: TRegExGroup);
BEGIN
  FTheGroup := AMatch;
END;


FUNCTION TGroupAsserter.AssertHasValue(CONST ValueExpected: STRING): TGroupAsserter;
BEGIN
  Assert.AreEqual(ValueExpected, FTheGroup.Value, 'for Group.Value');

  Result := self;
END;


FUNCTION TGroupAsserter.AssertIsSuccess(SuccessExpected: BOOLEAN): TGroupAsserter;
BEGIN
  Assert.AreEqual(SuccessExpected, FTheGroup.Success, 'for Group.Success');

  Result := self;
END;


FUNCTION TGroupAsserter.AssertIsZeroLengthAt(StartExpected: Integer): TGroupAsserter;
BEGIN
  self.AssertIsSuccess;
  self.AssertStartLength(StartExpected, 0);
  self.AssertHasValue('');

  Result := Self;
END;


FUNCTION TGroupAsserter.AssertMatchAt(StartExpected: Integer; CONST ValueExpected: STRING): TGroupAsserter;
BEGIN
  self.AssertIsSuccess;
  self.AssertStartLength(StartExpected, Length(ValueExpected));
  self.AssertHasValue(ValueExpected);

  Result := Self;
END;


FUNCTION TGroupAsserter.AssertStartLength(StartExpected, LengthExpected: Integer): TGroupAsserter;
BEGIN
  VAR StopExpected := StartExpected + LengthExpected;

  Assert.AreEqual(StartExpected , FTheGroup.Start , 'for Group.Start');
  Assert.AreEqual(StopExpected  , FTheGroup.Stop  , 'for Group.Stop');
  Assert.AreEqual(LengthExpected, FTheGroup.Length, 'for Group.Length');

  Result := self;
END;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TImmutableRegExTest);

END.
