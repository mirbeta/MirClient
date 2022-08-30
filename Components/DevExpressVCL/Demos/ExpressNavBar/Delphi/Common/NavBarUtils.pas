unit NavBarUtils;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, ImgList, ShellAPI;

const
  dxDownloadURL = 'http://www.devexpress.com/downloads';
  dxSupportURL = 'http://www.devexpress.com/Support/Center';
  dxStartURL = 'http://www.devexpress.com';
  dxProductsURL = 'http://www.devexpress.com/products';
  dxMyDXURL = 'http://www.mydevexpress.com';

type
  dxSitePage = (spDownloads, spSupport, spStart, spProducts, spMyDX);

  TdmCommonData = class(TDataModule)
    ilMain: TImageList;
    alMain: TActionList;
    actDownloads: TAction;
    actSupport: TAction;
    actDXOnTheWeb: TAction;
    actProducts: TAction;
    actExit: TAction;
    procedure actExitExecute(Sender: TObject);
    procedure actDownloadsExecute(Sender: TObject);
    procedure actSupportExecute(Sender: TObject);
    procedure actDXOnTheWebExecute(Sender: TObject);
    procedure actProductsExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure Browse(ASitePage: dxSitePage);
  
var
  dmCommonData: TdmCommonData;

implementation

{$R *.DFM}

procedure Browse(ASitePage: dxSitePage);
var
  AURL: string;
begin
  case ASitePage of
    spDownloads: AURL := dxDownloadURL;
    spSupport: AURL := dxSupportURL;
    spStart: AURL := dxStartURL;
    spProducts: AURL := dxProductsURL;
    spMyDX: AURL := dxMyDXURL;
  end;
  ShellExecute(0, 'OPEN', PChar(AURL), nil, nil, SW_SHOW);
end;

procedure TdmCommonData.actExitExecute(Sender: TObject);
begin
  Application.MainForm.Close;
end;

procedure TdmCommonData.actDownloadsExecute(Sender: TObject);
begin
  Browse(spDownloads);
end;

procedure TdmCommonData.actSupportExecute(Sender: TObject);
begin
  Browse(spSupport);
end;

procedure TdmCommonData.actDXOnTheWebExecute(Sender: TObject);
begin
  Browse(spStart);
end;

procedure TdmCommonData.actProductsExecute(Sender: TObject);
begin
  Browse(spProducts);
end;

end.
