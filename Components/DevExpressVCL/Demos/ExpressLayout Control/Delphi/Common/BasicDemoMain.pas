unit BasicDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxLayoutControl, cxControls, StdCtrls, cxLookAndFeels,
  dxLayoutLookAndFeels, ExtCtrls, Menus, DB, DemoDM,
  dxLayoutControlAdapters, DBCtrls, Mask, Grids, DBGrids, ActnList,
  cxGraphics, cxLookAndFeelPainters, dxLayoutContainer;

const
  dxDownloadURL = 'http://www.devexpress.com/downloads';
  dxSupportURL = 'http://www.devexpress.com/Support/Center';
  dxStartURL = 'http://www.devexpress.com';
  dxProductsURL = 'http://www.devexpress.com/products';
  dxMyDXURL = 'http://www.mydevexpress.com';

type
  dxSitePage = (spProducts, spDownloads, spStart, spSupport, spMyDX);

  TfrmBasicDemoMain = class(TForm)
    lcMain: TdxLayoutControl;
    mmMain: TMainMenu;
    miStyle: TMenuItem;
    Standard1: TMenuItem;
    Office1: TMenuItem;
    Web1: TMenuItem;
    UsecxLookAndFeel1: TMenuItem;
    UltraFlat1: TMenuItem;
    Flat1: TMenuItem;
    Standard2: TMenuItem;
    Office111: TMenuItem;
    Native1: TMenuItem;
    File1: TMenuItem;
    Customization1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    Aboutthisdemo1: TMenuItem;
    DeveloperExpressProducts1: TMenuItem;
    N2: TMenuItem;
    DeveloperExpressDownloads1: TMenuItem;
    DeveloperExpressontheWeb1: TMenuItem;
    SupportCenter1: TMenuItem;
    lcMainGroup_Root1: TdxLayoutGroup;
    alMain: TActionList;
    acLayoutStandard: TAction;
    acLayoutOffice: TAction;
    acLayoutWeb: TAction;
    acUltraFlat: TAction;
    acFlat: TAction;
    acOffice11: TAction;
    acStandard: TAction;
    acNative: TAction;
    Options1: TMenuItem;
    Autosize1: TMenuItem;
    aAutosize: TAction;
    procedure Customization1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure DeveloperExpressProducts1Click(Sender: TObject);
    procedure Aboutthisdemo1Click(Sender: TObject);
    procedure LayoutStyleExecute(Sender: TObject);
    procedure aAutosizeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  frmBasicDemoMain: TfrmBasicDemoMain;

implementation

{$R *.dfm}

uses
  ShellAPI, AboutDemoForm;

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

procedure TfrmBasicDemoMain.Customization1Click(Sender: TObject);
begin
  lcMain.Customization := True;
end;

procedure TfrmBasicDemoMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmBasicDemoMain.DeveloperExpressProducts1Click(Sender: TObject);
begin
  Browse(dxSitePage((Sender as TMenuItem).Tag));
end;

procedure TfrmBasicDemoMain.Aboutthisdemo1Click(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TfrmBasicDemoMain.LayoutStyleExecute(Sender: TObject);
var
  ATag: Integer;
begin
  ATag := (Sender as TAction).Tag;
  case ATag of
    0:
      lcMain.LayoutLookAndFeel := dmDemo.dxLayoutStandardLookAndFeel1;
    1:
      lcMain.LayoutLookAndFeel := dmDemo.dxLayoutOfficeLookAndFeel1;
    2:
      lcMain.LayoutLookAndFeel := dmDemo.dxLayoutWebLookAndFeel1;
  else
    lcMain.LayoutLookAndFeel := dmDemo.dxLayoutCxLookAndFeel1;
    case ATag of
      3..6:
        begin
          dmDemo.dxLayoutCxLookAndFeel1.LookAndFeel.NativeStyle := False;
          dmDemo.dxLayoutCxLookAndFeel1.LookAndFeel.Kind := TcxLookAndFeelKind(ATag - 3);
        end;
      7:
        dmDemo.dxLayoutCxLookAndFeel1.LookAndFeel.NativeStyle := True;
    end;
  end;
end;

procedure TfrmBasicDemoMain.aAutosizeExecute(Sender: TObject);
begin
  if aAutosize.Checked then
  begin
    lcMain.Align := alNone;
    lcMainGroup_Root1.AlignHorz := ahLeft;
    lcMainGroup_Root1.AlignVert := avTop;
    lcMain.AutoSize := True;
    AutoSize := True;
  end
  else
  begin
    AutoSize := False;
    lcMain.AutoSize := False;
    lcMainGroup_Root1.AlignHorz := ahClient;
    lcMainGroup_Root1.AlignVert := avClient;
    lcMain.Align := alClient;
  end;
end;

procedure TfrmBasicDemoMain.FormCreate(Sender: TObject);
begin
  if miStyle.Visible then
    case Tag of
      0: acLayoutStandard.Execute;
      1: acUltraFlat.Execute;
    end;
end;

end.
