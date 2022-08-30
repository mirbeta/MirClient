program PuzzleDemoD103Rio;

uses
  Forms,
  Puzzle in 'Puzzle.pas' {frmPuzzle};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'PuzzleDemoD103Rio';
  Application.CreateForm(TfrmPuzzle, frmPuzzle);
  Application.Run;
end.
