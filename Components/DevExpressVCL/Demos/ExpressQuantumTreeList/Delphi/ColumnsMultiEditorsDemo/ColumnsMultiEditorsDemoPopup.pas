unit ColumnsMultiEditorsDemoPopup;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, cxStyles, cxTL, cxControls,
  cxInplaceContainer, ExtCtrls, cxTextEdit, ImgList, cxDropDownEdit,
  cxClasses;

type
  TColumnsMultiEditorsDemoPopupForm = class(TForm)
    pnlPopup: TPanel;
    ilPoupuImages: TImageList;
    tlPopup: TcxTreeList;
    clText: TcxTreeListColumn;
    cxStyleRepository1: TcxStyleRepository;
    stlHotRoot: TcxStyle;
    stlContenet: TcxStyle;
    stlHotItem: TcxStyle;
    procedure tlPopupGetNodeImageIndex(Sender: TcxCustomTreeList;
      ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType;
      var AIndex: TImageIndex);
    procedure tlPopupHotTrackNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode;
      AShift: TShiftState; var ACursor: TCursor);
    procedure tlPopupClick(Sender: TObject);
    procedure tlPopupStylesGetHotTrackStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn;
      ANode: TcxTreeListNode; var AStyle: TcxStyle);
    procedure tlPopupKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FPopupEdit: TcxPopupEdit;
    procedure ClosePopupForm(Accept: Boolean);
  public
    { Public declarations }
    property PopupEdit: TcxPopupEdit read FPopupEdit write FPopupEdit;
  end;

implementation

{$R *.dfm}

procedure TColumnsMultiEditorsDemoPopupForm.tlPopupGetNodeImageIndex(
  Sender: TcxCustomTreeList; ANode: TcxTreeListNode;
  AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
begin

  if AIndexType = tlitStateIndex then Exit;
  if ANode.Level = 0 then
  begin
    AIndex := 0;
    if ANode.Expanded then
      Inc(AIndex);
  end
  else
     AIndex := 4;
  if ANode.HotTrack then
    if ANode.Level = 0 then
      Inc(AIndex, 2)
    else
      Inc(AIndex);
end;

procedure TColumnsMultiEditorsDemoPopupForm.tlPopupHotTrackNode(
  Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AShift: TShiftState;
  var ACursor: TCursor);
begin
  if ANode.Level = 1 then
    ACursor := crHandPoint
  else
    ACursor := crDefault;
end;

procedure TColumnsMultiEditorsDemoPopupForm.tlPopupClick(Sender: TObject);
begin
  if (tlPopup.FocusedNode <> nil) and (tlPopup.FocusedNode.Level = 1) then
    ClosePopupForm(True);
end;

procedure TColumnsMultiEditorsDemoPopupForm.ClosePopupForm(
  Accept: Boolean);
begin
  if PopupEdit <> nil then
  begin
    PopupEdit.DroppedDown := False;
    if Accept then
    begin
      if tlPopup.FocusedNode <> nil then
      begin
        PopupEdit.EditingText := tlPopup.FocusedNode.Values[0];
      end;
    end;
  end;
end;

procedure TColumnsMultiEditorsDemoPopupForm.tlPopupStylesGetHotTrackStyle(
  Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; 
  var AStyle: TcxStyle);
begin
  if ANode.Level = 0 then
    AStyle := stlHotRoot
  else
    AStyle := stlHotItem;
end;

procedure TColumnsMultiEditorsDemoPopupForm.tlPopupKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ClosePopupForm(False);
  if Key = VK_RETURN then
    tlPopupClick(nil);
end;

end.
