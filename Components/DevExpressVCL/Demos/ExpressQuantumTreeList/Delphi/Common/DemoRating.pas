unit DemoRating;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, StdCtrls, cxButtons, cxTextEdit, cxMemo,
  Controls, cxControls, cxContainer, cxEdit, cxRadioGroup, Classes,
  Variants, Graphics, Forms, Dialogs, cxLookAndFeelPainters;

type
  TDemoRatingForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    rgRate: TcxRadioGroup;
    memRateDescrip: TcxMemo;
    Label4: TLabel;
    btnSend: TcxButton;
    procedure rgRatePropertiesChange(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
    { Private declarations }
  end;

const
  OurEmail = 'Support@devexpress.com';
  EmailSubj = 'EQTreeList.Demos.';

var
  DemoRatingForm: TDemoRatingForm;

implementation

uses
  ComObj, DemoBasicMain, ShellAPI;

{$R *.dfm}

procedure TDemoRatingForm.rgRatePropertiesChange(
  Sender: TObject);
begin
  if rgRate.ItemIndex <> -1 then
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
    ASubj := EmailSubj + ChangeFileExt(ExtractFileName(Application.ExeName),'');
    ABody := 'Rate: ' + IntToStr(rgRate.ItemIndex + 1) + #13#10#13#10 +
    'Description:'#13#10 + memRateDescrip.Text;
    AdjustMessageBody('%', '$prc$');
    AdjustMessageBody('$prc$', '%25');
    AdjustMessageBody(#13#10, '%0D%0A');
    AdjustMessageBody('&', '%26');
    AdjustMessageBody(' ', '%20');
    ShellExecute(Handle, PChar('OPEN'), PChar('mailto:' + OurEmail + '?subject=' +
      ASubj + '&body=' + ABody) , nil, nil, SW_SHOWMAXIMIZED);
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
