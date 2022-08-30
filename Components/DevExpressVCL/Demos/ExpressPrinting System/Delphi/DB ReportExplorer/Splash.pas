unit Splash;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TfmSplash = class(TForm)
    lblText: TLabel;
    pnlImageHost: TPanel;
    Image1: TImage;
    Bevel1: TBevel;
    btnOK: TButton;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.DFM}

procedure TfmSplash.FormCreate(Sender: TObject);
const
  CRLF = #13#10;
  WarningText: string = 
    '   If you do not own appropriate Control Libraries from Developer Express Inc., part of Reports ' + CRLF + 
    'cannot be used, because last ones are needed specific ReportItems for rendering from these Libraries.' + CRLF + 
    '   Mentioned above ReportItems are the part of ReportLinks for these Libraries.' + CRLF +
     CRLF +
     CRLF +
    '   In case you own them, just uncomment follow units in uses clause of "Main.pas"' + CRLF +
     CRLF +
    '     - dxPScxSSLnk for ExpressSpreadSheet' + CRLF +
    '     - dxPSdxLC2Lnk for ExpressLayoutControl' + CRLF +
    '     - dxPScxCommon for ExpressEditors' + CRLF + 
    '       (also needed for ExpressQuantumGrid , ExpressQuantumTree and ExpressVerticalGrid)' + CRLF +
    '     - dxPSExtCommon for ExpressExtendedEditors';
begin
  Caption := Application.Title;
  lblText.Caption := WarningText;
end;

end.
