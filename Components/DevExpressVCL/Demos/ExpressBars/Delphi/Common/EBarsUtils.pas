unit EBarsUtils;

interface

uses
  SysUtils, Classes, Forms, ActnList, ImgList, Controls;

const
  dxDownloadURL = 'http://www.devexpress.com/downloads';
  dxSupportURL = 'http://www.devexpress.com/Support/Center';
  dxStartURL = 'http://www.devexpress.com';
  dxProductsURL = 'http://www.devexpress.com/products';
  dxMyDXURL = 'https://www.devexpress.com/ClientCenter';

type
  dxSitePage = (spDownloads, spSupport, spStart, spProducts, spMyDX);

  TdmCommonData = class(TDataModule)
    alMain: TActionList;
    actDockingHelp: TAction;
    actRateDemo: TAction;
    actDownloads: TAction;
    actSupport: TAction;
    actDXOnTheWeb: TAction;
    actProducts: TAction;
    actAbout: TAction;
    actBarsHelp: TAction;
    actMyDX: TAction;
    actSBarsHelp: TAction;
    procedure actSBarsHelpExecute(Sender: TObject);
    procedure actDockingHelpExecute(Sender: TObject);
    procedure actBarsHelpExecute(Sender: TObject);
    procedure actRateDemoExecute(Sender: TObject);
    procedure actMyDXExecute(Sender: TObject);
    procedure actDownloadsExecute(Sender: TObject);
    procedure actDXOnTheWebExecute(Sender: TObject);
    procedure actSupportExecute(Sender: TObject);
    procedure actProductsExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
  end;

procedure Browse(ASitePage: dxSitePage);

var
  dmCommonData: TdmCommonData;

implementation

{$R *.dfm}

uses
  Windows, ShellAPI, EBarsDemoRating, AboutDemoForm;

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

procedure TdmCommonData.actSBarsHelpExecute(Sender: TObject);
begin
  Application.HelpFile := '..\..\..\..\ExpressSideBar\Help\eSideBar.hlp';
  Application.HelpCommand(HELP_FINDER, 0);
end;

procedure TdmCommonData.actDockingHelpExecute(Sender: TObject);
begin
  Application.HelpFile := '..\..\..\..\ExpressDocking Library\Help\docking.hlp';
  Application.HelpCommand(HELP_FINDER, 0);
end;

procedure TdmCommonData.actBarsHelpExecute(Sender: TObject);
begin
  Application.HelpFile := '..\..\..\Help\ExpressBars.hlp';
  Application.HelpCommand(HELP_FINDER, 0);
end;

procedure TdmCommonData.actRateDemoExecute(Sender: TObject);
begin
  with TEBarsDemoRatingForm.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TdmCommonData.actMyDXExecute(Sender: TObject);
begin
  Browse(spMyDX);
end;

procedure TdmCommonData.actDownloadsExecute(Sender: TObject);
begin
  Browse(spDownloads);
end;

procedure TdmCommonData.actDXOnTheWebExecute(Sender: TObject);
begin
  Browse(spStart);
end;

procedure TdmCommonData.actSupportExecute(Sender: TObject);
begin
  Browse(spSupport);
end;

procedure TdmCommonData.actProductsExecute(Sender: TObject);
begin
  Browse(spProducts);
end;

procedure TdmCommonData.actAboutExecute(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

end.
