unit InPlaceEditorsDemoText;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, InPlaceEditorsDemoFrameManager, ExtCtrls;

type
  TfrmTextEditors = class(TEditorDemoBaseFrame)
    PaintBox1: TPaintBox;
    procedure PaintBox1Paint(Sender: TObject);
  private
    FCompany, FWWW, FPhone: string;
  public
    procedure SetParameters(ACompany, AHyperLink, APhone: string);
  end;

implementation

{$R *.dfm}

{ TfrmTextEdits }

procedure TfrmTextEditors.SetParameters(ACompany, AHyperLink, APhone: string);
begin
  FCompany := Format('COMPANY: %s', [ACompany]);
  FWWW := Format('WWW: %s', [AHyperLink]);
  FPhone := Format('PHONE: %s', [APhone]);
  PaintBox1.Invalidate;
end;

procedure TfrmTextEditors.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Brush.Color := clWindow;
  PaintBox1.Canvas.FillRect(PaintBox1.ClientRect);
  DrawText([FCompany, FWWW, FPhone], PaintBox1);
end;

end.
 
