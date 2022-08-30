unit FeaturesDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TfmFeaturesDailog = class(TForm)
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
  fmFeaturesDailog: TfmFeaturesDailog;

implementation

{$R *.dfm}

procedure TfmFeaturesDailog.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
