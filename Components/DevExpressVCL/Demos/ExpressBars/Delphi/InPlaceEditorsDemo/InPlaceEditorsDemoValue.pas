unit InPlaceEditorsDemoValue;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxGeometry, ExtCtrls, ComCtrls,
  InPlaceEditorsDemoFrameManager;

type
  TfrmValueEditors = class(TEditorDemoBaseFrame)
    PaintBox1: TPaintBox;
    procedure PaintBox1Paint(Sender: TObject);
  private
    FDate, FTime, FValue: string;
    FFontName: string;
    FFontSize: Integer;
  public
    procedure SetParameters(AFontSize: Integer; ADate, ATime: string; AMoney: Currency);
  end;

implementation

{$R *.dfm}

{ TfrmValueEdits }

procedure TfrmValueEditors.SetParameters(AFontSize: Integer; ADate, ATime: string; AMoney: Currency);
begin
  FFontName := 'Times New Roman';
  FFontSize := (AFontSize + 8) div 2;
  FDate := Format('Date: %s', [ADate]);
  FTime := Format('Time: %s', [ATime]);
  FValue := Format('Value: %m', [AMoney]);

  PaintBox1.Invalidate;
end;

procedure TfrmValueEditors.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Font.Name := FFontName;
  PaintBox1.Canvas.Font.Size := FFontSize;
  DrawText([FDate, FTime, FValue], PaintBox1);
end;

end.
