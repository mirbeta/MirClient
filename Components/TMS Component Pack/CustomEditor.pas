unit CustomEditor;

interface

{$I TMSDEFS.INC}

uses
  Windows, Forms, Messages, SysUtils, Variants, Graphics, Controls,
  Dialogs, GdipBase, StdCtrls, Classes, TypInfo, Grids, ValEdit, ComCtrls,
  AdvGDIP, AdvSmoothFillPreview, AdvSmoothFillEditor, ExtCtrls,
  GDIPFill, GDIPCustomItem, AdvPolyList, Contnrs, CustomItemsContainer, AdvStyleIF,
  Buttons,
{$IFDEF DELPHI6_LVL}
  DesignIntf,
  DesignEditors
{$ELSE}
  DsgnIntf,
{$ENDIF}
  ;

type
  TFrmCustomEditor = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    FontDialog1: TFontDialog;
    AdvPolyList1: TAdvPolyList;
    AdvPolyList2: TAdvPolyList;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Label22: TLabel;
    Label23: TLabel;
    GroupBox1: TGroupBox;
    PaintBox1: TPaintBox;
    Button3: TButton;
    Label1: TLabel;
    Button4: TButton;
    PaintBox2: TPaintBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button7: TButton;
    Button6: TButton;
    Button5: TButton;
    PaintBox3: TPaintBox;
    PaintBox4: TPaintBox;
    PaintBox5: TPaintBox;
    GroupBox2: TGroupBox;
    Button13: TButton;
    PaintBox7: TPaintBox;
    PaintBox6: TPaintBox;
    Button14: TButton;
    Button17: TButton;
    PaintBox8: TPaintBox;
    PaintBox9: TPaintBox;
    Button16: TButton;
    PaintBox10: TPaintBox;
    Button15: TButton;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    GroupBox3: TGroupBox;
    Button12: TButton;
    Label10: TStaticText;
    Label9: TStaticText;
    Button11: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Label8: TStaticText;
    Label7: TStaticText;
    Label6: TStaticText;
    Label11: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    ComboBox1: TComboBox;
    Label21: TLabel;
    SpeedButton5: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure SuperList1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SuperList1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SuperList1MouseLeave(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure PaintBox7Paint(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure CustomItemsContainer1ListItemStartDraw(Sender: TObject;
      AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF);
    procedure CustomItemsContainer2ListItemStartDraw(Sender: TObject;
      AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF);
    procedure CustomItemsContainer2MouseLeave(Sender: TObject);
    procedure CustomItemsContainer2MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure CustomItemsContainer2MouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CustomItemsContainer1DragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure CustomItemsContainer1DragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AdvPolyList1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AdvPolyList2DblClick(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
  private
    FOriginalList: TCustomBaseList;
    FFormDesigner: IDesigner;
    FHoveredItemIndex, FListHoveredItemIndex, FSelectedItemIndex, FListSelectedItemIndex: Integer;
    FDropItem: TCustomItem;
    { Private declarations }
  protected
    procedure FillPreviewClicked(Sender: TObject);
    procedure UpdateContainer;
    procedure ContainerChanged(Sender: TObject);
  public
    { Public declarations }
    procedure ListChanged(Sender: TObject);
    procedure ItemChange(Sender: TObject);
    procedure ItemSelect(Sender: TObject; Index: integer);
    procedure ItemDestroy(Sender: TObject);
    procedure ListDestroy(Sender: TObject);
    procedure RefreshList;
    procedure Init;
    procedure CorrectTop(lbl: TStaticText);
    procedure RefreshRegisteredClasses;
    property OriginalList : TCustomBaseList read FOriginalList write FOriginalList;
    property FormDesigner: IDesigner read FFormDesigner write FFormDesigner;
    procedure AddNewItem(ItemClassString: String; NewIndex: Integer; X, Y: integer);
    procedure DeleteItem;
  end;

  TDesignerSelectionsList = class(TDesignerSelections);

var
  FrmCustomEditor: TFrmCustomEditor;

implementation

{$R *.dfm}

{ TFrmCustomEditor }

procedure TFrmCustomEditor.AddNewItem(ItemClassString: String; NewIndex: integer; X, Y: integer);
var
  it: TCustomItem;
begin
  it := TCustomItem(FormDesigner.CreateComponent(OriginalList.GetItemClassByCustomName(ItemClassString),
    OriginalList.GetOwnerComponent, 0, 0, 0, 0));
  it.ItemOwner := OriginalList.GetOwnerComponent;
  if Assigned(it.Control) then
  begin
    if it.ItemOwner is TWinControl then
      it.Control.Parent := TWinControl(it.ItemOwner);
  end;
  OriginalList.AssignEvents(it);
  OriginalList.Items.Add(it);
  OriginalList.Changed;
  if NewIndex <> -1 then
    it.Index := NewIndex;
  FSelectedItemIndex := it.Index;
  AdvPolyList1.ScrollToItem(FSelectedItemIndex);
  it.X := X;
  it.Y := Y;
  RefreshList;
  AdvPolyList1.Invalidate;
  FormDesigner.SelectComponent(it);
end;

procedure TFrmCustomEditor.AdvPolyList1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (AdvPolyList1.Columns = 0) and (AdvPolyList1.Rows = 0) then
  begin
    if (FSelectedItemIndex >= 0) and (FSelectedItemIndex <= AdvPolyList1.List.Items.Count - 1) then
    begin
      with TCustomItem(OriginalList.Items[FSelectedItemIndex]) do
      begin
        case Key of
          VK_LEFT:
          begin
            if ssShift in Shift then
              X := X - 10
            else
              X := X - 1;
          end;
          VK_UP:
          begin
            if ssShift in Shift then
              Y := Y - 10
            else
              Y := Y - 1;
          end;
          VK_RIGHT:
          begin
            if ssShift in Shift then
              X := X + 10
            else
              X := X + 1;
          end;
          VK_DOWN:
          begin
            if ssShift in Shift then
              Y := Y + 10
            else
              Y := Y + 1;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFrmCustomEditor.AdvPolyList2DblClick(Sender: TObject);
begin
  if FListSelectedItemIndex <> -1 then
  begin
    AddNewItem((AdvPolyList2.List.Items[FListSelectedItemIndex] as TCustomItem).CustomClassName, -1, 0, 0);
  end;
end;

procedure TFrmCustomEditor.Button12Click(Sender: TObject);
var
  f: TFont;
  lbl: TStaticText;
begin
  f := nil;
  lbl := nil;
  case (Sender as TButton).Tag of
    5:
    begin
      f := OriginalList.Appearance.NormalFont;
      lbl := Label6;
    end;
    6:
    begin
      f := OriginalList.Appearance.DownFont;
      lbl := Label7;
    end;
    7:
    begin
      f := OriginalList.Appearance.DisabledFont;
      lbl := Label8;
    end;
    8:
    begin
      f := OriginalList.Appearance.HoveredFont;
      lbl := Label9;
    end;
    9:
    begin
      f := OriginalList.Appearance.SelectedFont;
      lbl := Label10;
    end;
  end;

  if Assigned(f) and Assigned(lbl) then
  begin
    FontDialog1.Font.Assign(f);
    if FontDialog1.Execute then
    begin
      f.Assign(FontDialog1.Font);
      lbl.font.assign(f);
      CorrectTop(lbl);
    end;
  end;
end;

procedure TFrmCustomEditor.Button13Click(Sender: TObject);
var
  f: TAdvSmoothFillEditorDialog;
begin
  f := TAdvSmoothFillEditorDialog.Create(Self);
  case (Sender as TButton).Tag of
  0: f.Fill := OriginalList.Appearance.ButtonNormal;
  1: f.Fill := OriginalList.Appearance.ButtonDown;
  2: f.Fill := OriginalList.Appearance.ButtonDisabled;
  3: f.Fill := OriginalList.Appearance.ButtonHovered;
  4: f.Fill := OriginalList.Appearance.ButtonSelected;
  end;
  f.Execute;
  RefreshList;
end;

procedure TFrmCustomEditor.Button3Click(Sender: TObject);
var
  f: TAdvSmoothFillEditorDialog;
begin
  f := TAdvSmoothFillEditorDialog.Create(Self);
  case (Sender as TButton).Tag of
  0: f.Fill := OriginalList.Appearance.Normal;
  1: f.Fill := OriginalList.Appearance.Down;
  2: f.Fill := OriginalList.Appearance.Disabled;
  3: f.Fill := OriginalList.Appearance.Hovered;
  4: f.Fill := OriginalList.Appearance.Selected;
  end;
  f.Execute;
  RefreshList;
end;

procedure TFrmCustomEditor.ComboBox1Change(Sender: TObject);
var
  tmsif: ITMSStyle;
  o: TComponent;
begin
  o := OriginalList.GetOwnerComponent;
  if Assigned(o) then
  begin
    if OriginalList.GetOwnerComponent.GetInterface(ITMSStyle, tmsif) then
    begin
      case ComboBox1.ItemIndex of
        0: tmsif.SetComponentStyle(tsOffice2003Blue);
        1: tmsif.SetComponentStyle(tsOffice2003Silver);
        2: tmsif.SetComponentStyle(tsOffice2003Olive);
        3: tmsif.SetComponentStyle(tsOffice2003Classic);
        4: tmsif.SetComponentStyle(tsOffice2007Luna);
        5: tmsif.SetComponentStyle(tsOffice2007Obsidian);
        6: tmsif.SetComponentStyle(tsOffice2007Silver);
        7: tmsif.SetComponentStyle(tsOffice2010Blue);
        8: tmsif.SetComponentStyle(tsOffice2010Silver);
        9: tmsif.SetComponentStyle(tsOffice2010Black);
        10: tmsif.SetComponentStyle(tsOffice2013White);
        11: tmsif.SetComponentStyle(tsOffice2013LightGray);
        12: tmsif.SetComponentStyle(tsOffice2013Gray);
        13: tmsif.SetComponentStyle(tsWindowsXP);
        14: tmsif.SetComponentStyle(tsWhidbey);
        15: tmsif.SetComponentStyle(tsWindowsVista);
        16: tmsif.SetComponentStyle(tsWindows7);
        17: tmsif.SetComponentStyle(tsWindows8);
        18: tmsif.SetComponentStyle(tsTerminal);
        19: tmsif.SetComponentStyle(tsWindows10);
        20: tmsif.SetComponentStyle(tsOffice2016White);
        21: tmsif.SetComponentStyle(tsOffice2016Gray);
        22: tmsif.SetComponentStyle(tsOffice2016Black);
      end;
    end;
  end;
end;

procedure TFrmCustomEditor.ContainerChanged(Sender: TObject);
begin
  UpdateContainer;
end;

procedure TFrmCustomEditor.CorrectTop(lbl: TStaticText);
begin
  lbl.Top := Round(lbl.Tag - (lbl.Font.Size - 8) / 2);
end;

procedure TFrmCustomEditor.CustomItemsContainer1DragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  it, itsel, itorig, itselorig: TCustomItem;
begin
  if (Source is TAdvPolyList) then
  begin
    AdvPolyList1.Invalidate;
    it := (Source as TAdvPolyList).SelectedDragDropItem;
    if Assigned(it) and (Source <> AdvPolyList1)  then
    begin
      if Assigned(FDropItem) then
      begin
        AddNewItem(it.CustomClassName, FDropItem.Index, X, Y);
        FDropItem := nil;
      end
      else
        AddNewItem(it.CustomClassName, -1, X, Y);
    end
    else
    begin
      it := AdvPolyList1.SelectedDragDropItem;
      itorig := nil;
      if Assigned(it) then
        itorig := TCustomItem(OriginalList.Items[it.Index]);

      itsel := AdvPolyList1.DropItem;
      itselorig := nil;
      if Assigned(itsel) then
        itselorig := TCustomItem(OriginalList.Items[itsel.Index]);

      if Assigned(itorig) then
      begin
        if Assigned(itselorig) then
        begin
          itorig.Index := itselorig.Index;
          it.Index := itsel.Index;
          FSelectedItemIndex := itsel.Index;
        end;
      end;
    end;
  end;
end;

procedure TFrmCustomEditor.CustomItemsContainer1DragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Source is TAdvPolyList) then
  begin
    Accept := true;
    if (Source = AdvPolyList1) then
    begin
      if (AdvPolyList1.Columns = 0) and (AdvPolyList1.Rows = 0) then
      begin
        if (FSelectedItemIndex >= 0) and (FSelectedItemIndex <= AdvPolyList1.List.Items.Count - 1) then
        begin
          AdvPolyList1.BeginUpdate;
          TCustomItem(OriginalList.Items[FSelectedItemIndex]).X := X;
          TCustomItem(OriginalList.Items[FSelectedItemIndex]).Y := Y;
          AdvPolyList1.EndUpdate;
        end;
      end;
    end;
  end
end;

procedure TFrmCustomEditor.CustomItemsContainer1ListItemStartDraw(
  Sender: TObject; AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF);
var
  r, ir: TGPRectF;
  rgn: TGPRegion;
  pth: TGPGraphicsPath;
begin
  r := ARect;
  ir := MakeRect(r.X - 4, r.Y - 4, r.Width + 9, r.Height + 9);
  rgn := TGPRegion.Create(ir);
  pth := TGPGraphicsPath.Create;
  try
    pth.AddRectangle(r);
    rgn.Exclude(pth);

    ir.Width := ir.Width - 1;
    ir.Height := ir.Height - 1;

    AGraphics.SetClip(rgn);

    if Item.Index = FSelectedItemIndex then
      OriginalList.Appearance.Selected.Fill(AGraphics, ir)
    else if Item.Index = FHoveredItemIndex then
      OriginalList.Appearance.Hovered.Fill(AGraphics, ir);

    AGraphics.ResetClip;
  finally
    rgn.Free;
    pth.Free;
  end;
end;

procedure TFrmCustomEditor.CustomItemsContainer2ListItemStartDraw(
  Sender: TObject; AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF);
var
  r, ir: TGPRectF;
  rgn: TGPRegion;
  pth: TGPGraphicsPath;
begin
  r := ARect;
  ir := MakeRect(r.X - 4, r.Y - 4, r.Width + 9, r.Height + 9);
  rgn := TGPRegion.Create(ir);
  pth := TGPGraphicsPath.Create;
  try
    pth.AddRectangle(r);
    rgn.Exclude(pth);

    ir.Width := ir.Width - 1;
    ir.Height := ir.Height - 1;

    AGraphics.SetClip(rgn);

    if Item.Index = FListSelectedItemIndex then
      OriginalList.Appearance.Selected.Fill(AGraphics, ir)
    else if Item.Index = FListHoveredItemIndex then
      OriginalList.Appearance.Hovered.Fill(AGraphics, ir);

    AGraphics.ResetClip;
  finally
    rgn.Free;
    pth.Free;
  end;
end;

procedure TFrmCustomEditor.CustomItemsContainer2MouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
  FListHoveredItemIndex := -1;
  AdvPolyList2.Invalidate;
end;

procedure TFrmCustomEditor.CustomItemsContainer2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  it: TCustomItem;
  fi: Integer;
begin
  it := AdvPolyList2.List.ItemAtXY(X, Y);

  if Assigned(it) then
  begin
    fi := it.Index;
  end
  else
    fi := -1;

  if FListHoveredItemIndex <> fi then
  begin
    FListHoveredItemIndex := fi;
    Screen.Cursor := crHandPoint;
    AdvPolyList2.Invalidate;
    Application.CancelHint;
  end
  else if fi = -1 then
    Screen.Cursor := crDefault;
end;

procedure TFrmCustomEditor.CustomItemsContainer2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  it: TCustomItem;
begin
  it := AdvPolyList2.List.ItemAtXY(X, Y);
  if Assigned(it) then
  begin
    FListSelectedItemIndex := it.Index;
    AdvPolyList2.Invalidate;
  end;
end;

procedure TFrmCustomEditor.DeleteItem;
var
  i: Integer;
begin
  if (FSelectedItemIndex >= 0) and (FSelectedItemIndex <= OriginalList.Items.Count - 1) then
  begin
    i := FSelectedItemIndex;
    OriginalList.RemoveItem(i);
    RefreshList;
    if OriginalList.Items.Count > 0 then
    begin
      if i < OriginalList.Items.Count - 1 then
        FSelectedItemIndex := i
      else
        FSelectedItemIndex := OriginalList.Items.Count - 1;

      if (FSelectedItemIndex >= 0) and (FSelectedItemIndex <= OriginalList.Items.Count - 1) then
        FormDesigner.SelectComponent(TCustomItem(OriginalList.Items[FSelectedItemIndex]));
    end
    else
      FSelectedItemIndex := -1;

    AdvPolyList1.ScrollToItem(FSelectedItemIndex);

    AdvPolyList1.Invalidate;
  end;
end;

procedure TFrmCustomEditor.FillPreviewClicked(Sender: TObject);
var
  fe: TAdvSmoothFillEditorDialog;
begin
  fe := TAdvSmoothFillEditorDialog.Create(Self);
  fe.Fill := (Sender as TAdvSmoothFillPreview).Fill;
  fe.Execute;
  FreeAndNil(fe);
end;

procedure TFrmCustomEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  o: TComponent;
begin
  o := OriginalList.GetOwnerComponent;
  if Assigned(o) then
  begin
    if o is TCustomItemsContainer then
    begin
      (o as TCustomItemsContainer).OnInternalChange := nil;
    end;
  end;
  OriginalList.OnInternalChange := nil;
  OriginalList.OnNotifyItemDestroy := nil;
  OriginalList.OnNotifyItemChange := nil;
  OriginalList.OnNotifyListDestroy := nil;
  OriginalList.OnNotifyItemDesignTimeSelect := nil;
  Action := caFree;
end;

procedure TFrmCustomEditor.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_DELETE: DeleteItem;
  end;
end;

procedure TFrmCustomEditor.FormShow(Sender: TObject);
begin
  AdvPolyList2.SetFocus;
end;

procedure TFrmCustomEditor.Init;
var
  o: TComponent;
begin
  o := OriginalList.GetOwnerComponent;
  if Assigned(o) then
  begin
    if o is TCustomItemsContainer then
    begin
      (o as TCustomItemsContainer).OnInternalChange := ContainerChanged;
    end;
  end;
  UpdateContainer;

  FHoveredItemIndex := -1;
  FSelectedItemIndex := -1;
  OriginalList.OnInternalChange := ListChanged;
  OriginalList.OnNotifyItemDestroy := ItemDestroy;
  OriginalList.OnNotifyItemChange := ItemChange;
  OriginalList.OnNotifyListDestroy := ListDestroy;
  OriginalList.OnNotifyItemDesignTimeSelect := ItemSelect;
  RefreshRegisteredClasses;
  RefreshList;
  if OriginalList.Items.Count > 0 then
  begin
    FSelectedItemIndex := 0;
    FormDesigner.SelectComponent(TCustomItem(OriginalList.Items[0]));
    AdvPolyList1.Invalidate;
  end;
  PageControl1.ActivePageIndex := 0;

  Label6.Font.Assign(OriginalList.Appearance.NormalFont);
  Label7.Font.Assign(OriginalList.Appearance.DownFont);
  Label8.Font.Assign(OriginalList.Appearance.DisabledFont);
  Label9.Font.Assign(OriginalList.Appearance.HoveredFont);
  Label10.Font.Assign(OriginalList.Appearance.SelectedFont);
  CorrectTop(Label6);
  CorrectTop(Label7);
  CorrectTop(Label8);
  CorrectTop(Label9);
  CorrectTop(Label10);
end;

procedure TFrmCustomEditor.ItemChange(Sender: TObject);
var
  I: integer;
  List: TDesignerSelectionsList;
begin
  List := TDesignerSelectionsList.Create;
  try
    FormDesigner.GetSelections(List);
    for I := 0 to List.Count - 1 do
    begin
      if List.Items[I] is TCustomItem then
      begin
        FSelectedItemIndex := OriginalList.Items.IndexOf(TCustomItem(List.Items[I]));
        Break;
      end;
    end;
    AdvPolyList1.Invalidate;
  finally
    List.Free;
  end;
end;

procedure TFrmCustomEditor.ItemDestroy(Sender: TObject);
begin
  RefreshList;
  if OriginalList.Items.Count > 0 then
  begin
    if FSelectedItemIndex >= OriginalList.Items.Count - 1 then
      FSelectedItemIndex := OriginalList.Items.Count - 1;
  end
  else
    FSelectedItemIndex := -1;

  AdvPolyList1.Invalidate;
end;

procedure TFrmCustomEditor.ItemSelect(Sender: TObject; Index: integer);
begin
  if (Index >= 0) and (Index <= OriginalList.Items.Count - 1) then
  begin
    FSelectedItemIndex := Index;
    AdvPolyList1.Invalidate;
    FormDesigner.SelectComponent(TCustomItem(OriginalList.Items[Index]));
  end;
end;

procedure TFrmCustomEditor.ListChanged(Sender: TObject);
begin
  RefreshList;
end;

procedure TFrmCustomEditor.ListDestroy(Sender: TObject);
begin
  Close;
end;

procedure TFrmCustomEditor.PaintBox1Paint(Sender: TObject);
var
  g: TGPGraphics;
  r: TGPRectF;
  p: TGPPen;
begin
  g := TGPGraphics.Create((Sender as TPaintBox).Canvas.Handle);
  p := TGPPen.Create(MakeColor(255, clSilver));
  try
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    r := MakeRect(1, 1, (Sender as TPaintBox).Width - 3, (Sender as TPaintBox).Height - 3);
    case (Sender as TPaintBox).Tag of
    0: OriginalList.Appearance.Normal.Fill(g, r);
    1: OriginalList.Appearance.Down.Fill(g, r);
    2: OriginalList.Appearance.Disabled.Fill(g, r);
    3: OriginalList.Appearance.Hovered.Fill(g, r);
    4: OriginalList.Appearance.Selected.Fill(g, r);
    end;
    g.DrawRectangle(p, r);
  finally
    p.Free;
    g.free;
  end;
end;

procedure TFrmCustomEditor.PaintBox7Paint(Sender: TObject);
var
  g: TGPGraphics;
  r: TGPRectF;
  p: TGPPen;
begin
  g := TGPGraphics.Create((Sender as TPaintBox).Canvas.Handle);
  p := TGPPen.Create(MakeColor(255, clSilver));
  try
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    r := MakeRect(1, 1, (Sender as TPaintBox).Width - 3, (Sender as TPaintBox).Height - 3);
    case (Sender as TPaintBox).Tag of
    0: OriginalList.Appearance.ButtonNormal.Fill(g, r);
    1: OriginalList.Appearance.ButtonDown.Fill(g, r);
    2: OriginalList.Appearance.ButtonDisabled.Fill(g, r);
    3: OriginalList.Appearance.ButtonHovered.Fill(g, r);
    4: OriginalList.Appearance.ButtonSelected.Fill(g, r);
    end;
    g.DrawRectangle(p, r);
  finally
    p.Free;
    g.free;
  end;
end;

procedure TFrmCustomEditor.RefreshList;
var
  I: Integer;
  it: TCustomItem;
begin
  AdvPolyList1.List.Items.Clear;
  AdvPolyList1.List.Assign(OriginalList);
  for I := 0 to AdvPolyList1.List.Items.Count - 1 do
  begin
    if AdvPolyList1.List.Items[i] is TCustomItem then
    begin
      it := TCustomItem(AdvPolyList1.List.Items[i]);
      it.OnInternalItemStartDraw := CustomItemsContainer1ListItemStartDraw;
    end;
  end;
end;

procedure TFrmCustomEditor.RefreshRegisteredClasses;
var
  l: TSubClassList;
  i: Integer;
  b: TObject;
  it: TCustomItem;
begin
  l := TSubClassList.Create;
  l.Execute(TCustomItem);

  for i := 0 to l.Count - 1 do
  begin
    b := l[i].NewInstance;
    if TCustomItem(b).Display then
    begin
      it := TCustomItem(b).CreateNewItem(AdvPolyList2);
      it.OnInternalItemStartDraw := CustomItemsContainer2ListItemStartDraw;
      it.initDesignTime;
      it.Hint := it.CustomClassName;
      it.UseCaption(it.CustomClassName);
      AdvPolyList2.List.AddItem(it);
      FreeAndNil(b);
    end;
  end;

  AdvPolyList2.List.Sort;
  FreeAndNil(l);
end;

procedure TFrmCustomEditor.SpeedButton1Click(Sender: TObject);
begin
  if FListSelectedItemIndex <> -1 then
  begin
    AddNewItem((AdvPolyList2.List.Items[FListSelectedItemIndex] as TCustomItem).CustomClassName, -1, 0, 0);
  end;
end;

procedure TFrmCustomEditor.SpeedButton2Click(Sender: TObject);
begin
  DeleteItem;
end;

procedure TFrmCustomEditor.SpeedButton3Click(Sender: TObject);
var
  it: TCustomItem;
begin
  if (FSelectedItemIndex >= 0) and (FSelectedItemIndex <= AdvPolyList1.List.Items.Count - 1) then
  begin
    it := TCustomItem(OriginalList.Items[FSelectedItemIndex]);
    it.Index := it.Index - 1;
    FSelectedItemIndex := it.Index;
    invalidate;
  end;
end;

procedure TFrmCustomEditor.SpeedButton4Click(Sender: TObject);
var
  it: TCustomItem;
begin
  if (FSelectedItemIndex >= 0) and (FSelectedItemIndex <= OriginalList.Items.Count - 1) then
  begin
    it := TCustomItem(OriginalList.Items[FSelectedItemIndex]);
    it.Index := it.Index + 1;
    FSelectedItemIndex := it.Index;
    invalidate;
  end;
end;

procedure TFrmCustomEditor.SpeedButton5Click(Sender: TObject);
var
  c: TCustomItem;
begin
  if (FSelectedItemIndex >= 0) and (FSelectedItemIndex <= AdvPolyList1.ItemCount - 1) then
  begin
    c := AdvPolyList1.List.Items[FSelectedItemIndex] as TCustomItem;
    AddNewItem(c.CustomClassName, -1, 0, 0);
    AdvPolyList1.Items[AdvPolyList1.ItemCount - 1].Assign(c);
  end;
end;

procedure TFrmCustomEditor.SuperList1MouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
  FHoveredItemIndex := -1;
  FDropItem := nil;
  AdvPolyList1.Invalidate;
end;

procedure TFrmCustomEditor.SuperList1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  it: TCustomItem;
  fi: Integer;
begin
  it := AdvPolyList1.List.ItemAtXY(X, Y);

  if Assigned(it) then
  begin
    fi := it.Index;
  end
  else
    fi := -1;

  if FHoveredItemIndex <> fi then
  begin
    FHoveredItemIndex := fi;
    Screen.Cursor := crHandPoint;
    AdvPolyList1.Invalidate;
  end
  else if fi = -1 then
    Screen.Cursor := crDefault;
end;

procedure TFrmCustomEditor.SuperList1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  it: TCustomItem;
begin
  it := AdvPolyList1.List.ItemAtXY(X, Y);
  if Assigned(it) then
  begin
    FSelectedItemIndex := it.Index;
    AdvPolyList1.Invalidate;
    FormDesigner.SelectComponent(TCustomItem(OriginalList.Items[FSelectedItemIndex]));
  end;
end;

procedure TFrmCustomEditor.UpdateContainer;
begin
  AdvPolyList1.BeginUpdate;
  if OriginalList.GetOwnerComponent is TCustomItemsContainer then
    AdvPolyList1.Assign((OriginalList.GetOwnerComponent as TCustomItemsContainer));

  AdvPolyList1.VerticalSpacing := 10;
  AdvPolyList1.HorizontalSpacing := 10;
  AdvPolyList1.ListMargins.Left := 10;
  AdvPolyList1.ListMargins.Top := 10;
  AdvPolyList1.ListMargins.Bottom := 10;
  AdvPolyList1.ListMargins.Right := 10;
  AdvPolyList1.ReadOnly := true;
  AdvPolyList1.Reorder := not ((AdvPolyList1.Columns = 0) and (AdvPolyList1.Rows = 0));

  AdvPolyList1.EndUpdate;

  PaintBox1.Invalidate;
  PaintBox2.Invalidate;
  PaintBox3.Invalidate;
  PaintBox4.Invalidate;
  PaintBox5.Invalidate;
  PaintBox6.Invalidate;
  PaintBox7.Invalidate;
  PaintBox8.Invalidate;
  PaintBox9.Invalidate;
  PaintBox10.Invalidate;
end;

end.
