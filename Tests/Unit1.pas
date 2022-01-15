UNIT Unit1;

INTERFACE

USES
  DUnitX.TestFramework,
  System.RegularExpressions,
  ThreadsafeRegex;

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
  END;

IMPLEMENTATION

TYPE TMatchAsserter = RECORD
  STRICT PRIVATE
    FTheMatch : TRegExMatch;
  PUBLIC
    CONSTRUCTOR Create(AMatch : TRegExMatch);

    FUNCTION AssertIsSuccess  (SuccessExpected : BOOLEAN = TRUE) : TMatchAsserter;
    FUNCTION AssertIsEmpty    (EmptyExpected   : BOOLEAN = TRUE) : TMatchAsserter;
    FUNCTION AssertHasValue   (CONST ValueExpected : STRING) : TMatchAsserter;
    FUNCTION AssertStartLength(StartExpected, LengthExpected : Integer) : TMatchAsserter;
END;


PROCEDURE TImmutableRegExTest.Test1;
BEGIN
  VAR R := TRegEx.Create('\b((?<word>\w+)\s*)+(?<end>[.?!])');
  VAR M := R.FirstMatch('This is a sentence. This is a second sentence.');

  Assert.IsTrue(M.Success);
  Assert.AreEqual(Length('This is a sentence.'), M.Length);
  Assert.AreEqual('This is a sentence.', M.Value);
  Assert.AreEqual(4, M.GroupCount);

  Assert.AreEqual('This is a sentence.', M.Group(0));
  Assert.AreEqual('sentence', M.Group(1));
  Assert.AreEqual('sentence', M.Group(2));
  Assert.AreEqual('.', M.Group(3));

  Assert.AreEqual('sentence', M.Group('word'));
  Assert.AreEqual('.', M.Group('end'));
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
END;


PROCEDURE TImmutableRegExTest.TestMatchAll2;
BEGIN
  VAR R := TRegEx.Create('.*');
  VAR M := R.Matches('hello');

  // .* - Match 1: 0-10 hello; Match 2: 10-10 null
  Assert.AreEqual(2, Length(M));

  TMatchAsserter.Create(M[0])
  .AssertIsSuccess
  .AssertHasValue('hello')
  .AssertStartLength(1, 5);

  // TODO Store Length instead of Stop

  TMatchAsserter.Create(M[1])
  .AssertIsSuccess
  .AssertIsEmpty
  .AssertHasValue('')
  .AssertStartLength(5, 0);


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


FUNCTION TMatchAsserter.AssertStartLength(StartExpected, LengthExpected : Integer) : TMatchAsserter;
BEGIN
  VAR StopExpected := StartExpected + LengthExpected;

  Assert.AreEqual(StartExpected , FTheMatch.Start , 'for Match.Start');
  Assert.AreEqual(StopExpected  , FTheMatch.Stop  , 'for Match.Stop');
  Assert.AreEqual(LengthExpected, FTheMatch.Length, 'for Match.Length');

  Result := self;
END;




INITIALIZATION
  TDUnitX.RegisterTestFixture(TImmutableRegExTest);

END.
