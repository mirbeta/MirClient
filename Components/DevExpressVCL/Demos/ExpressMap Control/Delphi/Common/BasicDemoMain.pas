unit BasicDemoMain;

interface

{$I cxVer.inc}

uses
{$IFDEF EXPRESSBARS}
  dxBar, dxStatusBar,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxMapControlTypes, dxMapControlBingMapImageryDataProvider,
  dxCustomMapItemLayer, dxMapItemLayer, cxClasses, dxMapLayer,
  dxMapImageTileLayer, dxMapControl, Menus, cxContainer, cxEdit, cxGroupBox;

type
  TfrmBasicDemoMain = class(TForm)
    dxMapControl1: TdxMapControl;
    cxLookAndFeelController1: TcxLookAndFeelController;
    mmMain: TMainMenu;
    miOptions: TMenuItem;
    cxGroupBox2: TcxGroupBox;
  private
    { Private declarations }
  public
  {$IFDEF EXPRESSBARS}
    BarManager: TdxBarManager;
  {$ENDIF}
    procedure AfterConstruction; override;
  end;

var
  frmBasicDemoMain: TfrmBasicDemoMain;

function DXBingKey: string;

implementation

uses
  DemoUtils, SkinDemoUtils;

{$R *.dfm}
{$R *.res}

function DXBingKey: string;
var
  Buffer: array [0..255] of Char;
begin
  SetString(Result, Buffer, LoadString(FindResourceHInstance(HInstance), 102, Buffer, Length(Buffer)));
end;

{ TForm2 }

procedure TfrmBasicDemoMain.AfterConstruction;
begin
  inherited;
{$IFDEF EXPRESSBARS}
  BarManager := TdxBarManager.Create(Self);
  dxBarConvertMainMenu(mmMain, BarManager);
  BarManager.Style := bmsUseLookAndFeel;
{$ENDIF}
{$IFDEF EXPRESSBARS}
  CreateSkinsMenuItem(BarManager);
{$ELSE}
  CreateSkinsMenuItem(mmMain);
{$ENDIF}
end;

end.
