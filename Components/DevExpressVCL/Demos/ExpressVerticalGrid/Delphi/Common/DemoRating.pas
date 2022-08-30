unit DemoRating;

interface

uses
  Windows, Messages, SysUtils, StdCtrls, Controls, Classes,
  Graphics, Forms, Dialogs, ExtCtrls, cxLookAndFeelPainters, cxButtons,
  cxControls, cxContainer, cxEdit, cxTextEdit, cxMemo, cxRadioGroup;

type
  TDemoRatingForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    memRateDescrip: TcxMemo;
    btnSend: TcxButton;
    rgRate: TcxRadioGroup;
    procedure rgRateChange(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  end;

const
  OurEmail = 'Support@devexpress.com';
  EmailSubj = 'cxVerticalGrid.Demos.';

var
  DemoRatingForm: TDemoRatingForm;

implementation

uses
  ShellAPI;

{$R *.dfm}

procedure TDemoRatingForm.rgRateChange(
  Sender: TObject);
begin
  if rgRate.ItemIndex  <> -1 then
    btnSend.Enabled := True;
end;

procedure TDemoRatingForm.btnSendClick(Sender: TObject);
var
  ABody, ASubj: string;
  procedure AdjustMessageBody(ASearchStr, AReplaceStr: string);
  var
    APos: integer;
  begin
    APos := Pos(ASearchStr,ABody);
    while APos <> 0 do
    begin
      Delete(ABody,APos, Length(ASearchStr));
      Insert(AReplaceStr,ABody, APos);
      APos := Pos(ASearchStr,ABody);
    end;
  end;
begin
  Screen.Cursor := crHourGlass;
  try
    ASubj := EmailSubj + ChangeFileExt(ExtractFileName(Application.ExeName),'')+'-user rating';
    ABody := 'Rate: ' + IntToStr(rgRate.ItemIndex + 1);
    if memRateDescrip.Text <> '' then
      ABody := ABody + #13#10#13#10 +'Description:'#13#10 + memRateDescrip.Text;
    AdjustMessageBody('%', '$prc$');
    AdjustMessageBody('$prc$', '%25');
    AdjustMessageBody(#13#10, '%0D%0A');
    AdjustMessageBody('&', '%26');
    AdjustMessageBody(' ', '%20');
    ShellExecute(Handle, PChar('OPEN'), PChar('mailto:' + OurEmail + '?subject=' +
      ASubj + '&body=' + ABody) , nil, nil, SW_SHOWMAXIMIZED);
  finally
    Screen.Cursor := crDefault;
    Close;
  end;
end;

end.
