{***************************************************************************}
{ TAdvDBComboBox Editor                                                     }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvDBComboBoxListEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ComCtrls, StdCtrls, AdvDBComboBox, DB, ExtCtrls;

type
  TComboListEditor = class(TForm)
    Panel1: TPanel;
    btn_Add: TButton;
    btn_Del: TButton;
    bt_ClearList: TButton;
    btn_GetStoredVal: TButton;
    btn_Ok: TButton;
    btn_Cancel: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    lbl_Images: TLabel;
    sg_ComboList: TStringGrid;
    lb_ImageList: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure bt_ClearListClick(Sender: TObject);
    procedure btn_OkClick(Sender: TObject);
    procedure btn_AddClick(Sender: TObject);
    procedure lb_ImageListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure sg_ComboListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btn_DelClick(Sender: TObject);
    procedure btn_GetStoredValClick(Sender: TObject);
    procedure sg_ComboListDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sg_ComboListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure sg_ComboListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
  private
    { Private declarations }
    FDBCombo: TAdvDBComboBox;
    procedure SetDBCombo(const Value: TAdvDBComboBox);
    procedure InitForm;
    procedure RemoveRows(RowIndex, RCount : Integer);
    procedure LoadImageListBox;
  public
    { Public declarations }
    property DBCombo: TAdvDBComboBox read FDBCombo write SetDBCombo;
  end;

var
  ComboListEditor: TComboListEditor;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TComboListEditor.RemoveRows(RowIndex, RCount: Integer);
var
  i: Integer;
begin
  if (sg_ComboList.RowCount > 0) then
  begin
    for I := 0 to sg_ComboList.ColCount - 1 do
      sg_ComboList.Cells[I, RowIndex] := '';

    for i := RowIndex to sg_ComboList.RowCount - 1 do
    begin
      if (i + RCount < sg_ComboList.RowCount) then
      begin
        sg_ComboList.Rows[i] := sg_ComboList.Rows[i + RCount];
        sg_ComboList.RowHeights[i] := sg_ComboList.RowHeights[i + RCount];
      end;
    end;

    sg_ComboList.RowCount := sg_ComboList.RowCount - RCount;

    if (sg_ComboList.RowCount < 2) then
    begin
      sg_ComboList.RowCount := 2;
      sg_ComboList.FixedRows := 1;

      for I := 0 to sg_ComboList.ColCount - 1 do
        sg_ComboList.Cells[I, 1] := '';
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboListEditor.btn_DelClick(Sender: TObject);
begin
  if (sg_ComboList.Row > 0) and Assigned(DBCombo) then
  begin
    RemoveRows(sg_ComboList.Row, 1);
  end;
  sg_ComboList.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TComboListEditor.btn_GetStoredValClick(Sender: TObject);
var
  OldActive: Boolean;
  sl: TStringList;
  aField: TField;
  I, j: Integer;
  cb: TBookmark;
begin
  if Assigned(DBCombo) and Assigned(DBCombo.DataSource) and Assigned(DBCombo.DataSource.DataSet) then
  begin
    aField := DBCombo.DataSource.DataSet.FieldByName(DBCombo.DataField);
    if Assigned(aField) then
    begin
      sl := TStringList.Create;
      sl.Duplicates := dupIgnore;
      sl.Sorted := True;
      OldActive := DBCombo.DataSource.DataSet.Active;
      DBCombo.DataSource.DataSet.DisableControls;
      DBCombo.DataSource.DataSet.Active := True;

      with DBCombo.DataSource.DataSet do
      begin
        try
          cb := GetBookMark;
          First;
          while not Eof do
          begin
            sl.Add(aField.Text);
            Next;
          end;
          GotoBookMark(cb);
          FreeBookMark(cb);
        finally
          DBCombo.DataSource.DataSet.EnableControls;
          DBCombo.DataSource.DataSet.Active := OldActive;
        end;
      end;

      //--- Delete already added values
      for I := 1 to sg_ComboList.RowCount - 1 do
      begin
        j := sl.IndexOf(sg_ComboList.Cells[1, I]);
        if (j >= 0) then
          sl.Delete(j);
      end;

      if (sl.Count > 0) then
      begin
        //--- Add rows
        for I := 1 to sl.Count do
        begin
          sg_ComboList.RowCount := sg_ComboList.RowCount + 1;
          sg_ComboList.Cells[0, sg_ComboList.RowCount - 1] := '';
          sg_ComboList.Cells[1, sg_ComboList.RowCount - 1] := '';
        end;
        //---

        for I := 1 to sg_ComboList.RowCount - 1 do
        begin
          if (sl.Count <= 0) then
            Break;
          
          if (sg_ComboList.Cells[1, I] = '') then
          begin
            sg_ComboList.Cells[1, I] := sl[0];
            sl.Delete(0);
          end;
        end;

        if (sg_ComboList.Cells[0, sg_ComboList.RowCount - 1] = '') and (sg_ComboList.Cells[1, sg_ComboList.RowCount - 1] = '') then
          sg_ComboList.RowCount := sg_ComboList.RowCount - 1;
      end;
      sl.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboListEditor.btn_OkClick(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(DBCombo) then
  begin
    DBCombo.Clear;
    for I := 1 to sg_ComboList.RowCount - 1 do
    begin
      if (sg_ComboList.Cells[0, I] <> '') or (sg_ComboList.Cells[1, I] <> '') then
        DBCombo.AddItem(sg_ComboList.Cells[0, I], sg_ComboList.Cells[1, I], nil);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboListEditor.bt_ClearListClick(Sender: TObject);
begin
  sg_ComboList.RowCount := 2;
  sg_ComboList.Cells[0, 1] := '';
  sg_ComboList.Cells[1, 1] := '';
  sg_ComboList.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TComboListEditor.btn_AddClick(Sender: TObject);
begin
  sg_ComboList.RowCount := sg_ComboList.RowCount + 1;
  sg_ComboList.Row := sg_ComboList.RowCount - 1;
  sg_ComboList.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TComboListEditor.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize := (NewWidth > 455) and (NewHeight > 300);
end;

procedure TComboListEditor.FormCreate(Sender: TObject);
begin
  FDBCombo := nil;
  InitForm;
end;

//------------------------------------------------------------------------------

procedure TComboListEditor.InitForm;
var
  I: Integer;
begin
  sg_ComboList.RowCount := 1;
  sg_ComboList.RowCount := 2;
  sg_ComboList.FixedRows := 1;

  sg_ComboList.Cells[0, 0] := 'Displayed Value';
  sg_ComboList.Cells[1, 0] := 'Stored Value';
  sg_ComboList.ColWidths[0] := 250;
  sg_ComboList.ColWidths[1] := 100;
  sg_ComboList.DefaultRowHeight := 20;

  if Assigned(DBCombo) and Assigned(DBCombo.Images) then
  begin
    Width := 566 + 2 * GetSystemMetrics(SM_CXBORDER);
    sg_ComboList.Width := 434;
    lb_ImageList.Visible := True;
    lb_ImageList.ItemHeight := DBCombo.Images.Height + 4;
    LoadImageListBox;
    btn_Ok.Left := 413;
    btn_Cancel.Left := 483;
    sg_ComboList.DefaultRowHeight := lb_ImageList.ItemHeight;
  end
  else
  begin
    Width := 456 + 2 * GetSystemMetrics(SM_CXBORDER);;
    sg_ComboList.Width := 434;
    lb_ImageList.Visible := False;
    btn_Ok.Left := 302;
    btn_Cancel.Left := 372;
  end;

  lbl_Images.Visible := lb_ImageList.Visible;

  if Assigned(DBCombo) then
  begin
    if DBCombo.Items.Count > 0 then
      sg_ComboList.RowCount := sg_ComboList.FixedRows + DBCombo.Items.Count;

    for I := 0 to DBCombo.Items.Count - 1 do
    begin
      sg_ComboList.Cells[0, I + 1] := DBCombo.Items.Strings[I];
      sg_ComboList.Cells[1, I + 1] := DBCombo.StoredStrings[I];
    end;
  end;


end;

//------------------------------------------------------------------------------

procedure TComboListEditor.lb_ImageListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  lb_ImageList.Canvas.FillRect(Rect);
  if Assigned(DBCombo) and Assigned(DBCombo.Images) then
  begin
    DBCombo.Images.Draw(lb_ImageList.Canvas, Rect.Left + 2, Rect.Top + 2, StrToInt(lb_ImageList.Items[Index]));
    lb_ImageList.Canvas.TextOut(Rect.Left + DBCombo.Images.Width + 6, Rect.Top+((lb_ImageList.ItemHeight- lb_ImageList.Canvas.TextHeight('X')) div 2), lb_ImageList.Items[Index]);
  end
  else
    lb_ImageList.Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, lb_ImageList.Items[Index]);
end;

//------------------------------------------------------------------------------

procedure TComboListEditor.LoadImageListBox;
var
  I: Integer;
begin
  if Assigned(DBCombo) and Assigned(DBCombo.Images) then
  begin
    lb_ImageList.Clear;
    for I := 0 to DBCombo.Images.Count - 1 do
      lb_ImageList.Items.Add(InttoStr(I));
  end;
end;

//------------------------------------------------------------------------------

procedure TComboListEditor.SetDBCombo(const Value: TAdvDBComboBox);
begin
  if (FDBCombo <> Value) then
  begin
    FDBCombo := Value;
    InitForm;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboListEditor.sg_ComboListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  r,c: integer;
begin
  sg_ComboList.MouseToCell(X,Y,c,r);

  sg_ComboList.Cells[c,r] := inttostr(lb_ImageList.ItemIndex);
end;

procedure TComboListEditor.sg_ComboListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  r,c: integer;
begin
  sg_ComboList.MouseToCell(X,Y,c,r);

  Accept := (c = 0) and (r > 0);
end;

procedure TComboListEditor.sg_ComboListDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  i,e: integer;
  c: string;
begin
  // custom draw when images are used
  if (ACol = 0) and (ARow > 0) then
  begin
    if Assigned(DBCombo) and Assigned(DBCombo.Images) and (DBCombo.ShowImages) then
    begin
      c := sg_ComboList.Cells[ACol,ARow];
      if (c <> '') then
      begin
        val(c, i, e);
        if (e = 0) and (i >= 0) and (i < DBCombo.Images.Count) then
          DBCombo.Images.Draw(sg_ComboList.Canvas, Rect.Right - 2 - DBCombo.Images.Width, Rect.Top + 2, i);
      end;
    end;
  end;
end;

procedure TComboListEditor.sg_ComboListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  I: Integer;
begin
  if (Key = VK_DOWN) and (sg_ComboList.Row = sg_ComboList.RowCount - 1) then
  begin
    sg_ComboList.RowCount := sg_ComboList.RowCount + 1;
    for I := 0 to sg_ComboList.ColCount - 1 do
      sg_ComboList.Cells[I, sg_ComboList.RowCount - 1] := '';
  end;
end;

//------------------------------------------------------------------------------

end.
