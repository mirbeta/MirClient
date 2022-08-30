unit AlphaBlendingDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TfmAlphaBlendingDailog = class(TForm)
    btnClose: TButton;
    Panel1: TPanel;
    RichEdit1: TRichEdit;
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmAlphaBlendingDailog: TfmAlphaBlendingDailog;

implementation

{$R *.dfm}

procedure TfmAlphaBlendingDailog.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
