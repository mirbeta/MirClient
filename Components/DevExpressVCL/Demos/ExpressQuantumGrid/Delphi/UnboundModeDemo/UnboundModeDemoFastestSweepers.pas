unit UnboundModeDemoFastestSweepers;

interface

uses
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, cxButtons, cxLookAndFeelPainters;

type
  TUnboundModeDemoFastestSweepersForm = class(TForm)
    lbBeginner: TLabel;
    lbIntermediate: TLabel;
    lbExpert: TLabel;
    lbExpertTime: TLabel;
    lbIntermediateTime: TLabel;
    lbBeginnerTime: TLabel;
    ibExpertName: TLabel;
    lbIntermediateName: TLabel;
    lbBeginnerName: TLabel;
    bntOK: TcxButton;
    btnResetScores: TcxButton;
    procedure btnResetScoresClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    FastestTimesResetted: Boolean;
    function ShowModal: Integer; override;
  end;

var
  UnboundModeDemoFastestSweepersForm: TUnboundModeDemoFastestSweepersForm;

implementation

uses UnboundModeDemoTypes;

{$R *.DFM}

function TUnboundModeDemoFastestSweepersForm.ShowModal: Integer;
begin
  SetFormPosition(Self, 35, 45);
  Result := inherited ShowModal;
end;

procedure TUnboundModeDemoFastestSweepersForm.btnResetScoresClick(Sender: TObject);
begin
  if FastestTimesResetted then Exit;
  FastestTimesResetted := True;
  lbBeginnerTime.Caption := IntToStr(999);
  lbIntermediateTime.Caption := IntToStr(999);
  lbExpertTime.Caption := IntToStr(999);
  lbBeginnerName.Caption := 'Anonymous';
  lbIntermediateName.Caption := 'Anonymous';
  ibExpertName.Caption := 'Anonymous';
end;

procedure TUnboundModeDemoFastestSweepersForm.FormCreate(Sender: TObject);
begin
  FastestTimesResetted := False;
end;

end.
