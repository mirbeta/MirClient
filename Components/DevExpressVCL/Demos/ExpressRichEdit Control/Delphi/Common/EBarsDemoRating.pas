unit EBarsDemoRating;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, StdCtrls, Controls, Classes,
  Graphics, Forms, Dialogs, ExtCtrls;

type
  TEBarsDemoRatingForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    memRateDescrip: TMemo;
    btnSend: TButton;
    rgRate: TRadioGroup;
    procedure rgRateChange(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  end;

const
  OurEmail = 'Support@devexpress.com';
  EmailSubj = 'EBars.Demos.';

var
  EBarsDemoRatingForm: TEBarsDemoRatingForm;

implementation

uses
  ShellAPI;

{$R *.dfm}

procedure TEBarsDemoRatingForm.rgRateChange(
  Sender: TObject);
begin
  if rgRate.ItemIndex  <> -1 then
    btnSend.Enabled := True;
end;

procedure TEBarsDemoRatingForm.btnSendClick(Sender: TObject);
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
