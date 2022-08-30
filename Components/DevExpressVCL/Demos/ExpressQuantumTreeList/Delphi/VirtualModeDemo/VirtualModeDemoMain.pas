unit VirtualModeDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, cxControls, cxLookAndFeels, ActnList,
  ImgList, Menus, ComCtrls, StdCtrls, DemoBasicMain, cxContainer, cxEdit,
  cxTextEdit, cxClasses, cxStyles, cxTL, cxSpinEdit, cxCalendar,
  cxInplaceContainer, cxTLData, cxGraphics, cxCustomData,
  cxLookAndFeelPainters;

const
  WM_TREELISTEXPANDED = WM_USER + 1;

type

  TfmVirtualModeDemoMain = class(TDemoBasicMainForm)
    StyleRepository: TcxStyleRepository;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    cxStyle8: TcxStyle;
    cxStyle9: TcxStyle;
    cxStyle10: TcxStyle;
    cxStyle11: TcxStyle;
    cxStyle12: TcxStyle;
    cxStyle13: TcxStyle;
    stlGroupNode: TcxStyle;
    stlFixedBand: TcxStyle;
    TreeListStyleSheetDevExpress: TcxTreeListStyleSheet;
    TreeList: TcxVirtualTreeList;
    clnId: TcxTreeListColumn;
    clnName: TcxTreeListColumn;
    clnDate: TcxTreeListColumn;
    N1: TMenuItem;
    miShowButtons: TMenuItem;
    miShowRoot: TMenuItem;
    miShowIndicator: TMenuItem;
    miShowTreeLines: TMenuItem;
    miCellAutoHeight: TMenuItem;
    miCellEndEllipsis: TMenuItem;
    miColumnAutoWidth: TMenuItem;
    N2: TMenuItem;
    miSmartLoadMode: TMenuItem;
    Operations1: TMenuItem;
    FullExpand1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure miShowTreeLinesClick(Sender: TObject);
    procedure miShowIndicatorClick(Sender: TObject);
    procedure miShowRootClick(Sender: TObject);
    procedure miShowButtonsClick(Sender: TObject);
    procedure miSmartLoadModeClick(Sender: TObject);
    procedure miCellAutoHeightClick(Sender: TObject);
    procedure miCellEndEllipsisClick(Sender: TObject);
    procedure miColumnAutoWidthClick(Sender: TObject);
    procedure TreeListExpanding(Sender: TcxCustomTreeList;
      ANode: TcxTreeListNode; var Allow: Boolean);
    procedure WMTreeListExpanded(var AMessage: TMEssage); message WM_TREELISTEXPANDED;
    procedure FullExpand1Click(Sender: TObject);
    procedure TreeListGetChildCount(Sender: TcxCustomTreeList;
      AParentNode: TcxTreeListNode; var ACount: Integer);
    procedure TreeListGetNodeValue(Sender: TcxCustomTreeList;
      ANode: TcxTreeListNode; AColumn: TcxTreeListColumn;
      var AValue: Variant);
    procedure TreeListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    FNodeCount: Integer;
    FStartExpanding: Boolean;
    FStartExpandingTick: Cardinal;
    procedure ShowLoadingTime(ALoadingTime: Integer);
    procedure ShowPerformance(AExpanded: Boolean);
    function MsecToStr(AMsec: Integer): string;
    function GetSmartLoad: Boolean;
    procedure SetSmartLoad(AValue: Boolean);
  public
    property SmartLoad: Boolean read GetSmartLoad write SetSmartLoad;
  end;

var
  fmVirtualModeDemoMain: TfmVirtualModeDemoMain;

implementation

uses
  ShellAPI, SkinDEmoUtils;

{$R *.dfm}

procedure TfmVirtualModeDemoMain.FormCreate(Sender: TObject);
begin
  inherited;
  sbMain.AutoHint := False;
  SmartLoad := True;
end;

procedure TfmVirtualModeDemoMain.miCellAutoHeightClick(Sender: TObject);
begin
  TreeList.OptionsView.CellAutoHeight := GetMenuItemChecked(Sender);
end;

procedure TfmVirtualModeDemoMain.miCellEndEllipsisClick(
  Sender: TObject);
begin
  TreeList.OptionsView.CellEndEllipsis := GetMenuItemChecked(Sender);
end;

procedure TfmVirtualModeDemoMain.miColumnAutoWidthClick(
  Sender: TObject);
begin
  TreeList.OptionsView.ColumnAutoWidth := GetMenuItemChecked(Sender);
end;

procedure TfmVirtualModeDemoMain.miShowTreeLinesClick(Sender: TObject);
begin
  TreeList.OptionsView.TreeLineStyle :=
    TcxTreeListTreeLineStyle(GetMenuItemChecked(Sender));
end;

procedure TfmVirtualModeDemoMain.miShowIndicatorClick(Sender: TObject);
begin
  TreeList.OptionsView.Indicator := GetMenuItemChecked(Sender);
end;

procedure TfmVirtualModeDemoMain.miShowRootClick(Sender: TObject);
begin
  TreeList.OptionsView.ShowRoot := GetMenuItemChecked(Sender);
end;

procedure TfmVirtualModeDemoMain.miShowButtonsClick(Sender: TObject);
begin
  TreeList.OptionsView.Buttons := GetMenuItemChecked(Sender);
end;

procedure TfmVirtualModeDemoMain.miSmartLoadModeClick(Sender: TObject);
begin
  SmartLoad := GetMenuItemChecked(Sender);
end;

procedure TfmVirtualModeDemoMain.ShowLoadingTime(ALoadingTime: Integer);
begin
  sbMain.Panels[1].Text := 'Loaded in ' + MsecToStr(ALoadingTime) + ' s';
  ShowPerformance(False);
end;

procedure TfmVirtualModeDemoMain.TreeListExpanding(
  Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  if not FStartExpanding then
  begin
    FStartExpanding := True;
    FNodeCount := TreeList.AbsoluteCount;
    FStartExpandingTick := GetTickCount;
    PostMessage(Handle, WM_TREELISTEXPANDED, 0, 0);
  end;
end;

procedure TfmVirtualModeDemoMain.ShowPerformance(AExpanded: Boolean);
begin
  sbMain.Panels[0].Text := 'Total nodes: ' + IntToStr(TreeList.AbsoluteCount);
  if AExpanded then
  begin
    sbMain.Panels[2].Text := 'Expanded in ' + MsecToStr(FStartExpandingTick) + ' s';
    if SmartLoad then
      sbMain.Panels[2].Text := sbMain.Panels[2].Text + ', ' +
        IntToStr(TreeList.AbsoluteCount - FNodeCount) +
        ' nodes have been created'
  end
  else
    SbMain.Panels[2].Text := '';
end;

function TfmVirtualModeDemoMain.MsecToStr(AMsec: Integer): string;
begin
  Result := Format('%2.3f', [AMsec / 1000]);
end;

procedure TfmVirtualModeDemoMain.WMTreeListExpanded(
  var AMessage: TMEssage);
begin
  FStartExpanding := False;
  FStartExpandingTick := GetTickCount - FStartExpandingTick;
  ShowPerformance(True);
end;

procedure TfmVirtualModeDemoMain.FullExpand1Click(Sender: TObject);
begin
  TreeList.FullExpand;
end;

procedure TfmVirtualModeDemoMain.TreeListGetChildCount(
  Sender: TcxCustomTreeList; AParentNode: TcxTreeListNode;
  var ACount: Integer);
begin
  if AParentNode.Level < 4 then
    ACount := 10;
end;

procedure TfmVirtualModeDemoMain.TreeListGetNodeValue(
  Sender: TcxCustomTreeList; ANode: TcxTreeListNode;
  AColumn: TcxTreeListColumn; var AValue: Variant);
begin
  case AColumn.ItemIndex of
    0:
      AValue := ANode.VisibleIndex;
    1:
      AValue := 'Level:' + IntToStr(ANode.Level);
    2:
      AValue := Now + ANode.VisibleIndex * 0.001
  else
    AValue := Null
  end;
end;

function TfmVirtualModeDemoMain.GetSmartLoad: Boolean;
begin
  Result := TreeList.OptionsData.SmartLoad;
end;

procedure TfmVirtualModeDemoMain.SetSmartLoad(AValue: Boolean);
var
  ALoadingTime: Cardinal;
begin
  ShowHourglassCursor;
  try
    TreeList.OnGetChildCount := nil;
    TreeList.OptionsData.SmartLoad := AValue;
    TreeList.OnGetChildCount := TreeListGetChildCount;
    ALoadingTime := GetTickCount;
    TreeList.FullRefresh;
    ShowLoadingTime(GetTickCount - ALoadingTime);
  finally
    HideHourglassCursor;
  end;
end;

procedure TfmVirtualModeDemoMain.TreeListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited;
  //
end;

end.
