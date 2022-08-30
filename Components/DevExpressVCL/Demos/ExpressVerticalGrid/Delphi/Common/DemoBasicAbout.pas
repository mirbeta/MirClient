unit DemoBasicAbout;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, jpeg, ComCtrls, cxLookAndFeelPainters,
  cxButtons, cxControls, cxContainer, cxEdit, cxImage;

type
  TcxGetDemoGetAboutEvent = procedure(AAboutText: TStrings) of object;
  TcxGetDemoGetCaptionEvent = procedure(var ACaption: string) of object;

  TDemoBasicAboutForm = class(TForm)
    lbDemoName: TLabel;
    lbCopyright: TLabel;
    bvBottom: TBevel;
    lbCompanyName: TLabel;
    reDemoInfo: TRichEdit;
    btnOK: TcxButton;
    cxImage: TcxImage;
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  end;

var
  DemoBasicAboutForm: TDemoBasicAboutForm;

implementation

{$R *.dfm}

procedure TDemoBasicAboutForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TDemoBasicAboutForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TDemoBasicAboutForm.FormDestroy(Sender: TObject);
begin
  DemoBasicAboutForm := nil;
end;

end.
