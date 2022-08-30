unit CheckGroupsDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxLookAndFeels, ActnList, ImgList, Menus, ComCtrls,
  StdCtrls, DemoBasicMain, cxContainer, cxEdit, cxTextEdit, cxStyles, cxTL,
  cxInplaceContainer, cxEditRepositoryItems, cxGraphics, cxCustomData,
  cxLookAndFeelPainters, cxClasses;

type
  TfmGheckGroupsDemo = class(TDemoBasicMainForm)
    tlDXInstallation: TcxTreeList;
    cxTreeList1Column1: TcxTreeListColumn;
    cxImageList1: TcxImageList;
    cxEditRepository1: TcxEditRepository;
    cxEditRepository1TextItem1: TcxEditRepositoryTextItem;
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    N1: TMenuItem;
    ShowTreeLines1: TMenuItem;
    procedure ShowTreeLines1Click(Sender: TObject);
  end;

var
  fmGheckGroupsDemo: TfmGheckGroupsDemo;

implementation

{$R *.dfm}

uses
  SkinDemoUtils;

procedure TfmGheckGroupsDemo.ShowTreeLines1Click(Sender: TObject);
begin
  if GetMenuItemChecked(Sender) then
    tlDXInstallation.OptionsView.TreeLineStyle := tllsDot
  else
    tlDXInstallation.OptionsView.TreeLineStyle := tllsNone;
end;

end.
