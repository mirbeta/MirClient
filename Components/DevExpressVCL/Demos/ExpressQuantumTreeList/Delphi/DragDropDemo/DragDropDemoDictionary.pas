unit DragDropDemoDictionary;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, DragDropDemoData,
  cxGraphics, cxCustomData, cxStyles, cxTL, cxControls, cxInplaceContainer,
  cxTLData, cxDBTL, cxMaskEdit, cxCurrencyEdit, cxMemo, cxCheckBox;

type
  TDragDropDemoDictionaryForm = class(TForm)
    pcDictionary: TPageControl;
    tsDepartments: TTabSheet;
    tsPersons: TTabSheet;
    tlDeptDict: TcxDBTreeList;
    tlEmplDict: TcxDBTreeList;
    lsc: TLabel;
    Label1: TLabel;
    tlDeptDictPARENTID: TcxDBTreeListColumn;
    tlDeptDictNAME: TcxDBTreeListColumn;
    tlDeptDictBUDGET: TcxDBTreeListColumn;
    tlDeptDictPHONE: TcxDBTreeListColumn;
    tlDeptDictFAX: TcxDBTreeListColumn;
    tlDeptDictEMAIL: TcxDBTreeListColumn;
    tlDeptDictVACANCY: TcxDBTreeListColumn;
    tlEmplDictName: TcxDBTreeListColumn;
    tlEmplDictCountry: TcxDBTreeListColumn;
    tlEmplDictPostalCode: TcxDBTreeListColumn;
    tlEmplDictCity: TcxDBTreeListColumn;
    tlEmplDictAddress: TcxDBTreeListColumn;
    tlEmplDictPhone: TcxDBTreeListColumn;
    tlEmplDictFax: TcxDBTreeListColumn;
    tlEmplDictEMAIL: TcxDBTreeListColumn;
    tlEmplDictHOMEPAGE: TcxDBTreeListColumn;
    tlEmplDictDepartmentID: TcxDBTreeListColumn;
    procedure tlDictDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tlDictMoveTo(Sender: TcxCustomTreeList;
      AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode;
      Nodes: TList; var IsCopy, Done: Boolean);
  public
    procedure SetDeptSelectionParentValue(AParentValue: Variant);
    procedure SetEmplSelectionDeptID(ADepartmentID: Variant);
  end;

procedure SetSelectedNodesValue(ATreeList: TcxDBTreeList; AItemIndex: Integer;
  AValue: Variant);

var
  DragDropDemoDictionaryForm: TDragDropDemoDictionaryForm;

implementation

{$R *.dfm}
function cxCompareNodes(AItem1, AItem2: Pointer): Integer;
begin
  with TcxDBTreeListNode(AItem1) do
    Result := TcxDBTreeListNode(AItem2).KeyValue - KeyValue;
end;

procedure SetSelectedNodesValue(ATreeList: TcxDBTreeList; AItemIndex: Integer; AValue: Variant);
var
  I: Integer;
  AList: TList;
begin
  with ATreeList do
  begin
    AList := TList.Create;
    BeginUpdate;
    try
      ATreeList.GetSelections(AList);
      AList.Sort(cxCompareNodes);
      for I := 0 to AList.Count - 1 do
        if TcxTreeListNode(AList[I]).Values[AItemIndex] <> AValue then
        begin

          TcxTreeListNode(AList[I]).Focused := True;
          DataController.Edit;
          TcxTreeListNode(AList[I]).Values[AItemIndex] := AValue;
          DataController.Post;
        end;
    finally
      AList.Free;
      EndUpdate;
    end;
  end
end;

procedure TDragDropDemoDictionaryForm.SetDeptSelectionParentValue(
  AParentValue: Variant);
begin
  SetSelectedNodesValue(tlDeptDict, tlDeptDictPARENTID.ItemIndex, AParentValue);
end;

procedure TDragDropDemoDictionaryForm.SetEmplSelectionDeptID(
  ADepartmentID: Variant);
begin
  SetSelectedNodesValue(tlEmplDict, tlEmplDictDepartmentID.ItemIndex, ADepartmentID);
end;

procedure TDragDropDemoDictionaryForm.tlDictDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
end;

procedure TDragDropDemoDictionaryForm.tlDictMoveTo(Sender: TcxCustomTreeList;
  AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode;
  Nodes: TList; var IsCopy, Done: Boolean);
begin
  Done := True;
end;

end.

