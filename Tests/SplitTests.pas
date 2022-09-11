UNIT SplitTests;

INTERFACE

USES
  DUnitX.TestFramework,
  System.RegularExpressions,
  ImmutableRegex;

TYPE
  [TestFixture]
  TImmutableRegExSplitTest = CLASS
  PUBLIC
    [Test]
    PROCEDURE SimpleSplit;
    [Test]
    PROCEDURE SplitsOnInputBounds;
    [Test]
    PROCEDURE RepeatedMatch;
    [Test]
    PROCEDURE CapturedText;
    [Test]
    PROCEDURE CapturedText2;
    [Test]
    PROCEDURE ZeroLengthMatches;

    [Test]
    PROCEDURE StartPosBeforeMatch;
    [Test]
    PROCEDURE StartPosAfterMatch;
    [Test]
    PROCEDURE StartPosAtMatch;
    [Test]
    PROCEDURE StartPosAfterDoubleMatch;


    // Tests for Count overload: Count <= 0, Count = 1, Count = Max Occ in String, Count > Max Occ in String
  END;

IMPLEMENTATION

PROCEDURE _AssertArrayEqual(CONST Expected : TArray<STRING>;
                            CONST Actual : TArray<STRING>);
BEGIN
  Assert.AreEqual(Length(Expected), Length(Actual));

  FOR VAR I := 0 TO Length(Expected)-1 DO BEGIN
    Assert.AreEqual(Expected[I], Actual[I]);
  END;
END;



{ TImmutableRegExSplitTest }


PROCEDURE TImmutableRegExSplitTest.SimpleSplit;
BEGIN
  // -
  // plum-pear
  VAR R := TRegEx.Create('-');
  VAR Split := R.Split('plum-pear');

  _AssertArrayEqual(['plum', 'pear'], Split);
END;


PROCEDURE TImmutableRegExSplitTest.SplitsOnInputBounds;
BEGIN
  VAR R := TRegEx.Create('a');

  System.RegularExpressions.TRegEx.Split('jhdfg', 'a');

  VAR EmptySplit := R.Split('');
  VAR Split1 := R.Split('a');
  VAR Split2 := R.Split('abc');
  VAR Split3 := R.Split('cba');
  VAR Split4 := R.Split('xyz');

  _AssertArrayEqual([], EmptySplit);   // .Net: [''], Delphi: []
  _AssertArrayEqual(['', ''], Split1);
  _AssertArrayEqual(['', 'bc'], Split2);
  _AssertArrayEqual(['cb', ''], Split3);
  _AssertArrayEqual(['xyz'], Split4);
END;


PROCEDURE TImmutableRegExSplitTest.CapturedText;
BEGIN
  VAR R := TRegEx.Create('(-)');
  VAR Split := R.Split('plum-pear');

  _AssertArrayEqual(['plum', '-', 'pear'], Split);
END;

PROCEDURE TImmutableRegExSplitTest.CapturedText2;
BEGIN
  VAR R := TRegEx.Create('(-)(p)');
  VAR Split := R.Split('plum-pear');

  _AssertArrayEqual(['plum', '-', 'p', 'ear'], Split);
END;


PROCEDURE TImmutableRegExSplitTest.RepeatedMatch;
BEGIN
  // -
  // plum--pear
  VAR R := TRegEx.Create('-');
  VAR Split := R.Split('plum--pear');

  _AssertArrayEqual(['plum', '', 'pear'], Split);
END;

PROCEDURE TImmutableRegExSplitTest.ZeroLengthMatches;
BEGIN
  VAR R := TRegEx.Create('\d*');

  VAR Split := R.Split('abc');

  _AssertArrayEqual(['', 'a', 'b', 'c', ''], Split);
END;


PROCEDURE TImmutableRegExSplitTest.StartPosBeforeMatch;
BEGIN
  VAR R := TRegEx.Create('-');

  VAR Split := R.Split('plum-pear', 999, 2);

  _AssertArrayEqual(['plum', 'pear'], Split);
END;


PROCEDURE TImmutableRegExSplitTest.StartPosAfterMatch;
BEGIN
  VAR R := TRegEx.Create('-');

  VAR Split := R.Split('plum-pear', 999, 7);

  _AssertArrayEqual(['plum-pear'], Split);
END;


PROCEDURE TImmutableRegExSplitTest.StartPosAtMatch;
BEGIN
  VAR R := TRegEx.Create('-');

  VAR Split := R.Split('plum-pear', 999, 5);

  _AssertArrayEqual(['plum', 'pear'], Split);
END;


PROCEDURE TImmutableRegExSplitTest.StartPosAfterDoubleMatch;
BEGIN
  VAR R := TRegEx.Create('-');

  VAR Split := R.Split('plum-pear-peach', 999, 6);

  _AssertArrayEqual(['plum-pear', 'peach'], Split);
END;

INITIALIZATION
  TDUnitX.RegisterTestFixture(TImmutableRegExSplitTest);

END.
