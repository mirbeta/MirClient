unit AboutDemoForm;

interface

{$I cxVer.inc}


uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls,
  cxControls, cxContainer, cxEdit, cxTextEdit, cxMemo, cxRichEdit, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters;

type
  TformAboutDemo = class(TForm)
    redDescription: TcxRichEdit;
  public
    constructor Create(const ADescription: string); reintroduce;
  end;

procedure ShowAboutDemoForm;

implementation

{$R *.dfm}

uses
  Types;

var
  FForm: TformAboutDemo;

procedure ShowAboutDemoForm;
var
  ADescription: TStringList;
begin
  if FForm = nil then
  begin
    ADescription := TStringList.Create;
    try
      ADescription.LoadFromFile(ExtractFilePath(Application.ExeName) + 'About.txt');
      FForm := TformAboutDemo.Create(ADescription.Text);
    finally
      ADescription.Free;
    end;
  end;
  FForm.Show;
end;

constructor TformAboutDemo.Create(const ADescription: string);

  procedure AssignBounds;
  var
    ADesktopArea: TRect;
    AOffset: Integer;
  begin
    Left := Application.MainForm.BoundsRect.Right;
    Top := Application.MainForm.BoundsRect.Top;
    Height := Application.MainForm.Height;
    ADesktopArea := GetDesktopWorkArea(Point(Left, Top));
    if BoundsRect.Right > ADesktopArea.Right then
    begin
      AOffset := BoundsRect.Right - ADesktopArea.Right;
      Left := Left - AOffset;
      if Application.MainForm.Left > AOffset then
        Application.MainForm.Left := Application.MainForm.Left - AOffset
      else
        Application.MainForm.Left := 0;
    end;
  end;

begin
  inherited Create(Application);
  AssignBounds;
  redDescription.Lines.Text := ADescription;
end;

end.
