{*******************************************************}
{               MiTeC Common Routines                   }
{              VCL controls extensions                  }
{                                                       }
{                                                       }
{         Copyright (c) 1997-2019 Michal Mutl           }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}


unit MiTeC_CtrlRtns;

interface

uses {$IFDEF RAD9PLUS}
     System.Classes, VCL.Controls, VCL.StdCtrls, VCL.ComCtrls, WinAPI.CommCtrl,
     WinAPI.Windows, VCL.Dialogs, System.SysUtils, VCL.Forms, VCL.Graphics, VCL.Grids,
     VCL.ImgList, WinAPI.UxTheme, VCL.Menus;
     {$ELSE}
     {$IFDEF THEMESUPPORT} UxTheme, {$ENDIF}
     Classes, Controls, StdCtrls, ComCtrls, CommCtrl, Windows, Dialogs, SysUtils,
     Forms, Graphics, Grids, ImgList, Menus;
     {$ENDIF}

function ComponentToString(Component: TComponent): string;

procedure SetWinControlStatus(Sender: TWinControl; Enabled: Boolean; OnColor: TColor = clWhite; OffColor: TColor = clBtnFace);

procedure ListView_SaveToFile(Sender :TListView; AFileName: string);
procedure ListView_LoadFromFile(Sender :TListView; AFileName: string);
procedure ListView_LoadStrings(SourceList :TStringList; AListItems: TListItems; ADelimiter :Char; AImageIndex :Integer); overload;
procedure ListView_LoadStrings(SourceList :TStrings; AListItems: TListItems; ADelimiter :Char; AImageIndex :Integer); overload;
function ListView_CustomSort(Item1, Item2: TListItem; AColumn: integer): Integer;
procedure ListView_DrawLine(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean; LineColor: TColor; const ALeftText: string = ''; const ARightText: string = '');
function ListView_GetCheckedCount(Sender: TListView): Integer;
procedure ListView_CheckAll(Sender: TListView; AState: boolean);
procedure ListView_DrawCheckBox(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean; TrueValue: string);
procedure ListView_DrawButton(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean; Text: string);
procedure ListView_DrawImage(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean; Bitmap: TBitmap; DrawText: Boolean = False; Text: string = ''); overload;
procedure ListView_DrawImage(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean; ImageList: TCustomImageList; ImageIndex: Integer; BgColor: TColor; DrawText: Boolean = False; Text: string = ''); overload;
procedure ListView_DrawBkg(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean; BkColor: TColor; Text: string = '');
procedure ListView_ExportToCSV(AList: TListView; AFilename: string; AIncludeHeader: boolean = True); overload;
procedure ListView_ExportToCSV(AList: TListView; AStrings: TStrings; AIncludeHeader: boolean = True); overload;
{$IFNDEF FPC}procedure ListView_Print(AList: TListView; ATitle,ASubTitle,AFooter: string);{$ENDIF}

function Form_Show(Sender: TFormClass): Boolean;
procedure Form_SetVisible(Sender :TForm);
procedure Form_SetInvisible(Sender :TForm);
procedure Form_HideCaption(Sender :TForm);
procedure Form_ShowCaption(Sender :TForm);
function Form_IsCaptionVisible(Sender :TForm): boolean;
procedure Form_Move(Sender: TWinControl);

function Tree_FindNode(Sender: TTreeView; AText: string): TTreeNode; overload;
function Tree_FindNode(AParent: TTreeNode; AText: string): TTreeNode; overload;
function Tree_FindNodeByPath(Sender: TTreeView; AText: string): TTreeNode;
function Tree_GetNodePath(Sender: TTreeView; ANode: TTreeNode): string;
function Tree_CreateNodeByPath(Sender: TTreeView; AText: string; ARoot: TTreeNode = nil; AImageIndex: Integer=0): TTreeNode;
procedure Tree_ExportToText(Sender: TTreeView; AFileName: string);

procedure Stat_SetText(Sender :TStatusBar; AIndex :integer; AText :string);

procedure StringGrid_Clear(grid: TStringGrid);
procedure StringGrid_SelectCell(grid: TStringGrid; ACol, ARow: integer);
procedure StringGrid_ExportToCSV(grid: TStringGrid; FileName: string);
procedure StringGrid_ImportCSV(grid: TStringGrid; FileName: string);

{$IFNDEF FPC}procedure ImageList_ConvertToHighColor(ImageList: TImageList);{$ENDIF}

function Edit_GetValue(AEdit: TCustomEdit): Double;
procedure Edit_SetValue(AEdit: TCustomEdit; AValue: Double);

procedure PageControl_HideAllTabs(pc: TPageControl);

procedure ComboBox_AddHistory(cb: TComboBox; AText: string = '');

procedure SetMainMenu(AMainMenu: TMainMenu; AEnabled: Boolean);

{
procedure DBGrid_DrawCheckBoxes(Canvas: TCanvas; const Rect: TRect; Field: TField; Color: TColor; Selected: Boolean; TrueValue: variant);
procedure DBGrid_DrawBitmaps(Canvas: TCanvas; const Rect: TRect; Field: TField; Color: TColor; Selected: Boolean; Bitmap: TBitmap; DrawText: boolean);
}

type
  TStretchMode = (smOnlySmaller, smOnlyBigger, smAny);

function StretchRect(AStart,ADest: TRect; ACenterX: Boolean = True; ACenterY: Boolean = True; APreserveRatio: Boolean = True; AMode: TStretchMode = smAny): TRect;

procedure ConvertICO2BMP(Icon: TIcon; TransColor: TColor; Bitmap: TBitmap);


const
  itemdelimiter = '|';

implementation

uses {$IFDEF RAD9PLUS}
     WinAPI.Messages, VCL.Printers,
     {$ELSE}
     Messages, Printers,
     {$ENDIF}
     MiTeC_StrUtils, MiTeC_Helpers;

var
  FullRgn, ClientRgn, CtlRgn : THandle;

{$IFDEF FPC}
function Point(AX, AY: Integer): TPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;
{$ENDIF}

function ComponentToString(Component: TComponent): string;
var
  BinStream: TMemoryStream;
  StrStream: TStringStream;
begin
  BinStream:=TMemoryStream.Create;
  try
    StrStream:=TStringStream.Create(Result);
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result:=StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;


procedure SetWinControlStatus;

procedure SetColor(Sender: TWinControl; Enabled: Boolean; OnColor, OffColor: TColor);
begin
  if Sender is TEdit then begin
    if Enabled then
      TEdit(Sender).Color:=OnColor
    else
      TEdit(Sender).Color:=OffColor;
  end else
    if Sender is TListBox then begin
      if Enabled then
        TListBox(Sender).Color:=OnColor
      else
        TListBox(Sender).Color:=OffColor;
    end else
      if Sender is TMemo then begin
        if Enabled then
          TMemo(Sender).Color:=OnColor
        else
          TMemo(Sender).Color:=OffColor;
      end else
        if Sender is TStringGrid then begin
          if Enabled then
            TStringGrid(Sender).Color:=OnColor
          else
            TStringGrid(Sender).Color:=OffColor;
        end else
          if Sender is TComboBox then begin
            if Enabled then
              TComboBox(Sender).Color:=OnColor
            else
              TComboBox(Sender).Color:=OffColor;
          end;
end;

var
  i: Integer;
begin
  Sender.Enabled:=Enabled;
  SetColor(Sender,Enabled,OnColor,OffColor);
  if csAcceptsControls in Sender.ControlStyle then
    for i:=0 to Sender.ControlCount-1 do begin
      if Sender.Controls[i] is TWinControl then begin
        SetColor(TWinControl(Sender.Controls[i]),Enabled,OnColor,OffColor);
        SetWinControlStatus(TWinControl(Sender.Controls[i]),Enabled,OnColor,OffColor);
      end;
    end;
end;

procedure ListView_SaveToFile;
var
  i,j,k: integer;
  F: TFileStream;
  pText: PChar;
  sText: String;
  w,ItemCount,SubCount: word;
  MySignature: array[0..2] of char;
begin
  ItemCount:=0;
  SubCount:=0;
  MySignature:='LVF';
  F:=TFileStream.Create(AFileName,fmCreate or fmOpenWrite);
  try
    F.Write(MySignature,sizeof(MySignature));
    ItemCount:=Sender.Items.Count;
    F.Write(ItemCount,SizeOf(ItemCount));
    if Sender.Items.Count>0 then begin
      for i:=1 to ItemCount do begin
        with Sender.Items[i-1] do begin
          SubCount:=SubItems.Count;
          F.Write(SubCount,SizeOf(SubCount));
          k:=ImageIndex;
          F.Write(k,Sizeof(k));
          sText:=Caption;
          w:=(Length(sText)+1)*SizeOf(Char);
          pText:=AllocMem(w);
          try
            StrPLCopy(pText,sText,Length(sText));
            F.Write(w,sizeof(w));
            F.Write(pText^,w);
          finally
            FreeMem(pText);
          end;
          if SubCount>0 then begin
            for j:=0 to SubItems.Count-1 do begin
              sText:=SubItems[j];
              w:=(Length(sText)+1)*SizeOf(Char);
              pText:=AllocMem(w);
              try
                StrPLCopy(pText,sText,Length(sText));
                F.Write(w,sizeof(w));
                F.Write(pText^,w);
              finally
                FreeMem(pText);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure ListView_LoadFromFile;
const
  itemdelimiter = '|';
var
  F: TFileStream;
  i,j,k: Integer;
  w,ItemCount,SubCount: Word;
  pText: PChar;
  PTemp: PChar;
  MySignature: array[0..2] of Char;
  n: TListItem;
  s: string;
begin
  ItemCount:=0;
  SubCount:=0;
  F:=TFileStream.Create(AFileName,fmOpenRead);
  try
    F.Read(MySignature,sizeof(MySignature));
    if MySignature<>'LVF' then
      Exit;
    F.Read(ItemCount,sizeof(ItemCount));
    for i:=1 to ItemCount do begin
      F.read(SubCount,SizeOf(SubCount));
      F.Read(k,sizeof(k));
      s:=IntToStr(k);
      F.Read(w,SizeOf(w));
      pText:=AllocMem(w);
      pTemp:=AllocMem(w);
      try
        F.Read(pTemp^,w);
        StrLCopy(pText,pTemp,w);
        s:=s+itemDelimiter+StrPas(pText);
        n:=Sender.Items.Add;
        n.Caption:=StrPas(pText);
        n.ImageIndex:=k;
      finally
        FreeMem(pTemp);
        FreeMem(pText);
      end;
      if SubCount>0 then begin
        for j:=1 to SubCount do begin
          F.Read(w,SizeOf(w));
          pText:=AllocMem(w);
          pTemp:=AllocMem(w);
          try
            F.Read(pTemp^,w);
            StrLCopy(pText,pTemp,w);
            s:=s+itemDelimiter+StrPas(pText);
            n.SubItems.Add(StrPas(pText));
          finally
            FreeMem(pTemp);
            FreeMem(pText);
          end;
        end;
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure ListView_LoadStrings(SourceList :TStringList; AListItems: TListItems;
  ADelimiter :Char; AImageIndex :Integer); overload;
var
  i,p :integer;
  s :string;
  n :tlistitem;
begin
  with AListItems do begin
    BeginUpdate;
    Clear;
    for i:=0 to SourceList.count-1 do begin
      s:=SourceList[i];
      if copy(s,length(s),1)<>ADelimiter then
        s:=s+ADelimiter;
      n:=add;
      if AImageIndex=-1 then begin
        p:=pos(ADelimiter,s);
        n.imageindex:=StrToInt(copy(s,1,p-1));
        System.delete(s,1,p);
      end;
      p:=pos(ADelimiter,s);
      n.caption:=copy(s,1,p-1);
      System.delete(s,1,p);
      if AImageIndex>-1 then
        n.imageindex:=AImageIndex;
      p:=pos(ADelimiter,s);
      while p>0 do begin
       n.subitems.add(copy(s,1,p-1));
       System.delete(s,1,p);
       p:=pos(ADelimiter,s);
      end;
    end;
    EndUpdate;
  end;
end;

procedure ListView_LoadStrings(SourceList :TStrings; AListItems: TListItems;
  ADelimiter :Char; AImageIndex :Integer); overload;
var
  i,p :integer;
  s :string;
  n :tlistitem;
begin
  with AListItems do begin
    BeginUpdate;
    Clear;
    for i:=0 to SourceList.count-1 do begin
      s:=SourceList[i];
      if copy(s,length(s),1)<>ADelimiter then
        s:=s+ADelimiter;
      n:=add;
      if AImageIndex=-1 then begin
        p:=pos(ADelimiter,s);
        n.imageindex:=StrToInt(copy(s,1,p-1));
        System.delete(s,1,p);
      end;
      p:=pos(ADelimiter,s);
      n.caption:=copy(s,1,p-1);
      System.delete(s,1,p);
      if AImageIndex>-1 then
        n.imageindex:=AImageIndex;
      p:=pos(ADelimiter,s);
      while p>0 do begin
       n.subitems.add(copy(s,1,p-1));
       System.delete(s,1,p);
       p:=pos(ADelimiter,s);
      end;
    end;
    EndUpdate;
  end;
end;

function ListView_CustomSort;
var
  Str1, Str2: string;
  Val1, Val2: double;
  Date1, Date2: TDateTime;
  Diff: TDateTime;
begin
  if (Item1=NIL) or (Item2=NIL) then begin
    Result:=0;
    exit;
  end;

  try
    if AColumn=0 then begin
      Str1:=Item1.Caption;
      Str2:=Item2.Caption;
    end else begin
      if AColumn<=Item1.SubItems.Count then
        Str1:=Item1.SubItems[AColumn-1]
      else
        Str1:='';
      if AColumn<=Item2.SubItems.Count then
        Str2:=Item2.SubItems[AColumn-1]
      else
        Str2:='';
    end;

    if IsValidDateTime(Str1,Date1) and IsValidDateTime(Str2,Date2) then begin
      Diff:=Date1-Date2;
      if Diff<0.0 then
        Result:=-1
      else
        if Diff>0.0 then
          Result:=1
        else
          Result:=0
    end else
      if IsValidNumber(Str1,Val1) and IsValidNumber(Str2,Val2) then begin
        if Val1<Val2 then
          Result:=-1
        else
          if Val1>Val2 then
            Result:=1
          else
            Result:=0
      end else
        Result:=AnsiCompareStr(Str1,Str2);
  except
    Result:=0;
  end;
end;

procedure ListView_DrawLine;
var
  Rect: TRect;
  p: TPoint;
  i,x,tw: Integer;
  c: TColor;
begin
  {$IFDEF THEMESUPPORT}
  if UseThemes then
    DefaultDraw:=False;
  {$ENDIF}
  with (Sender as TListView) do begin
    p:=Item.Position;
    x:=0;
    for i:=0 to Columns.Count-1 do
      x:=x+Column[i].Width;
    Rect.Top:=p.y;
    Rect.Left:=p.x;
    Rect.Bottom:=Rect.Top+16;
    Rect.Right:=Rect.Left+x;

    c:=Canvas.Brush.Color;
    if (cdsFocused in State) then begin
      Canvas.Brush.Color:=clHighlight;
      if (cdsHot in State) then
        Canvas.Brush.Color:=clHotlight;
      if not Assigned(Selected) then
        Canvas.Brush.Color:=c;
    end else
      Canvas.Brush.Color:=c;
    Canvas.Pen.Color:=Canvas.Brush.Color;
    {$IFDEF THEMESUPPORT}
    if not UseThemes then
    {$ENDIF}
      Canvas.Rectangle(Rect);
    if Canvas.Brush.Color<>c then
      Canvas.Pen.Color:=c
    else
      Canvas.Pen.Color:=LineColor;
    Canvas.MoveTo(Rect.Left,((Rect.Bottom - Rect.Top) div 2) + Rect.Top);
    Canvas.LineTo(Rect.Left+Rect.Right-5,((Rect.Bottom - Rect.Top) div 2) + Rect.Top);
    Canvas.Brush.Color:=c;
    if ALeftText<>'' then begin

    end;
    if ARightText<>'' then begin
      tw:=Canvas.TextWidth(ARightText);
      Canvas.TextOut(Rect.Right-tw-10,Rect.Top,ARightText);
    end;
  end;
end;

procedure ListView_DrawBkg;
var
  Rect: TRect;
  p: TPoint;
  i,x: Integer;
  c: TColor;
begin
  with (Sender as TListView) do begin
    p:=Item.Position;
    x:=0;
    for i:=0 to Columns.Count-1 do
      x:=x+Column[i].Width;
    Rect.Top:=p.y;
    Rect.Left:=p.x;
    Rect.Bottom:=Rect.Top+Canvas.TextHeight('W')+2;
    Rect.Right:=Rect.Left+x;

    c:=Canvas.Brush.Color;
    Canvas.Brush.Color:=BkColor;
    if (cdsFocused in State) then begin
      Canvas.Brush.Color:=clHighlight;
      if (cdsHot in State) then
        Canvas.Brush.Color:=clHotlight;
      if not Assigned(Selected) then
        Canvas.Brush.Color:=BkColor;
    end else
      Canvas.Brush.Color:=BkColor;
    Canvas.FillRect(Rect);
    if Text<>'' then
      Canvas.TextOut(Rect.Left+10,(Rect.Bottom-Rect.Top-Canvas.TextHeight('W')) div 2,Text);
    Canvas.Brush.Color:=c;
  end;
end;

function ListView_GetCheckedCount;
var
  i: integer;
begin
  Result:=0;
  with Sender, Items do
    for i:=0 to Count-1 do
      if Items[i].Checked then
        Inc(Result);
end;

procedure ListView_CheckAll(Sender: TListView; AState: boolean);
var
  i: integer;
begin
  with Sender, Items do
    for i:=0 to Count-1 do
      Items[i].Checked:=AState;
end;


procedure ListView_DrawCheckBox;
var
  Rect,MyRect: TRect;
  p: TPoint;
  i,x: Integer;
  pc,bc: TColor;
begin
  {$IFDEF THEMESUPPORT}
  if UseThemes then
    DefaultDraw:=False;
  {$ENDIF}
  with (Sender as TListView) do begin
    p:=Item.Position;
    x:=0;
    for i:=0 to SubItem-1 do
      x:=x+Column[i].Width;
    Rect.Top:=p.y;
    Rect.Left:=p.x+x;
    Rect.Bottom:=Rect.Top+16;
    Rect.Right:=Rect.Left+Column[SubItem].Width;

    bc:=Canvas.Brush.Color;
    pc:=Canvas.Pen.Color;
    if (RowSelect or (SubItem=0)) then
      if (cdsFocused in State) then begin
        Canvas.Brush.Color:=clHighlight;
        if (cdsHot in State) then
          Canvas.Brush.Color:=clHotlight;
        if not Assigned(Selected) then
          Canvas.Brush.Color:=bc;
      end else
        Canvas.Brush.Color:=bc;

    Canvas.Pen.Color:=Canvas.Brush.Color;

    {$IFDEF THEMESUPPORT}
    if not UseThemes then
    {$ENDIF}
      Canvas.Rectangle(Rect);

    Canvas.Pen.Color:=Canvas.Font.Color;

    MyRect.Top:=((Rect.Bottom - Rect.Top - 11) div 2) + Rect.Top;
    MyRect.Bottom:=MyRect.Top + 10;
    case Column[SubItem].Alignment of
      taLeftJustify: MyRect.Left:=Rect.Left+5;
      taCenter: MyRect.Left:=((Rect.Right-Rect.Left-11) div 2)+Rect.Left;
      taRightJustify: MyRect.Left:=Rect.Right-20;
    end;
    MyRect.Right:=MyRect.Left+10;

    Canvas.Brush.Color:=clWhite;
    {$IFDEF THEMESUPPORT}
    if not UseThemes then
    {$ENDIF}
      Canvas.FillRect(MyRect);

    Canvas.Polyline([
      Point(MyRect.Left, MyRect.Top), Point(MyRect.Right, MyRect.Top),
      Point(MyRect.Right, MyRect.Bottom), Point(MyRect.Left, MyRect.Bottom),
      Point(MyRect.Left, MyRect.Top)]);

    if Item.SubItems[SubItem-1]=TrueValue then begin
      Canvas.MoveTo(MyRect.Left + 2, MyRect.Top + 4);
      Canvas.LineTo(MyRect.Left + 2, MyRect.Top + 7);
      Canvas.MoveTo(MyRect.Left + 3, MyRect.Top + 5);
      Canvas.LineTo(MyRect.Left + 3, MyRect.Top + 8);
      Canvas.MoveTo(MyRect.Left + 4, MyRect.Top + 6);
      Canvas.LineTo(MyRect.Left + 4, MyRect.Top + 9);
      Canvas.MoveTo(MyRect.Left + 5, MyRect.Top + 5);
      Canvas.LineTo(MyRect.Left + 5, MyRect.Top + 8);
      Canvas.MoveTo(MyRect.Left + 6, MyRect.Top + 4);
      Canvas.LineTo(MyRect.Left + 6, MyRect.Top + 7);
      Canvas.MoveTo(MyRect.Left + 7, MyRect.Top + 3);
      Canvas.LineTo(MyRect.Left + 7, MyRect.Top + 6);
      Canvas.MoveTo(MyRect.Left + 8, MyRect.Top + 2);
      Canvas.LineTo(MyRect.Left + 8, MyRect.Top + 5);
    end;
    Canvas.Brush.Color:=bc;
    Canvas.Pen.Color:=pc;
  end;
end;

procedure ListView_DrawButton;
var
  Rect,MyRect: TRect;
  p: TPoint;
  i,x,w,fs: Integer;
  bc,pc: TColor;
  fn: string;
begin
  with (Sender as TListView) do begin
    p:=Item.Position;
    x:=0;
    for i:=0 to SubItem-1 do
      x:=x+Column[i].Width;
    Rect.Top:=p.y;
    Rect.Left:=p.x+x;
    Rect.Bottom:=Rect.Top+16;
    Rect.Right:=Rect.Left+Column[SubItem].Width;

    fn:=Canvas.Font.Name;
    fs:=Canvas.Font.Size;
    bc:=Canvas.Brush.Color;
    pc:=Canvas.Pen.Color;
    if (RowSelect or (SubItem=0)) then
      if (cdsFocused in State) then begin
        Canvas.Brush.Color:=clHighlight;
        if (cdsHot in State) then
          Canvas.Brush.Color:=clHotlight;
        if not Assigned(Selected) then
          Canvas.Brush.Color:=bc;
        Canvas.Pen.Color:=clWhite;
      end else begin
        Canvas.Brush.Color:=bc;
        Canvas.Pen.Color:=Canvas.Font.Color;
      end;

    Canvas.Font.Name:='Small Fonts';
    Canvas.Font.Size:=6;
    w:=Canvas.TextWidth(Text)+6;

    MyRect.Top:=((Rect.Bottom - Rect.Top -13) div 2) + Rect.Top;
    MyRect.Left:=(Rect.Right - Rect.Left - w+2) + Rect.Left - 3;
    MyRect.Bottom:=MyRect.Top + 11;
    MyRect.Right:=MyRect.Left + w - 3;

    Canvas.Brush.Color:=clBtnFace;
    {$IFDEF THEMESUPPORT}
    if not UseThemes then
    {$ENDIF}
      Canvas.Rectangle(MyRect);

    Canvas.Pen.Color:=clWhite;
    Canvas.Polyline([
      Point(MyRect.Left, MyRect.Bottom), Point(MyRect.Left, MyRect.Top),
      Point(MyRect.Right, MyRect.Top)]);
    Canvas.Pen.Color:=cl3DDkShadow;
    Canvas.Polyline([
      Point(MyRect.Right, MyRect.Top), Point(MyRect.Right, MyRect.Bottom),
      Point(MyRect.Left, MyRect.Bottom)]);

    InflateRect(MyRect,-1,-1);

    if w>0 then
      Canvas.TextRect(MyRect,MyRect.Left,MyRect.Top,Text);

    Canvas.Brush.Color:=bc;
    Canvas.Pen.Color:=pc;
    Canvas.Font.Size:=fs;
    Canvas.Font.Name:=fn;
  end;
end;

procedure ListView_DrawImage(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean; Bitmap: TBitmap; DrawText: Boolean = False; Text: string = ''); overload;
var
  Rect, MyRect: TRect;
  p: TPoint;
  i,x: Integer;
  c: TColor;
begin
  with (Sender as TListView) do begin
    p:=Item.Position;
    x:=0;
    for i:=0 to SubItem-1 do
      x:=x+Column[i].Width;
    Rect.Top:=p.y;
    Rect.Left:=p.x+x;
    Rect.Bottom:=Rect.Top+16;
    Rect.Right:=Rect.Left+Column[SubItem].Width;

    c:=Canvas.Brush.Color;
    if (RowSelect or (SubItem=0)) then
      if (cdsFocused in State) then begin
        Canvas.Brush.Color:=clHighlight;
        if (cdsHot in State) then
          Canvas.Brush.Color:=clHotlight;
        if not Assigned(Selected) then
          Canvas.Brush.Color:=c;
      end else
        Canvas.Brush.Color:=c;

    {$IFDEF THEMESUPPORT}
    if not UseThemes then
    {$ENDIF}
      Canvas.Rectangle(Rect);

    MyRect.Top:={((Rect.Bottom - Rect.Top - 16) div 2) + }Rect.Top+1;
    MyRect.Left:={((Rect.Right - Rect.Left - 11) div 2) +} Rect.Left+1;
    MyRect.Bottom:=MyRect.Top + 15;
    MyRect.Right:=MyRect.Left + 14;

    if Canvas.Brush.Color<>clWhite then
      Canvas.Pen.Color:=clWhite
    else
      Canvas.Pen.Color:=clBlack;

    if Item.Selected then
      Canvas.CopyMode:=cmSrcCopy
    else
      Canvas.CopyMode:=cmSrcCopy;
    Canvas.StretchDraw(MyRect,Bitmap);
    if DrawText and (Text<>'') then
      Canvas.TextOut(MyRect.Left+20,Rect.Top+2,Text);

    Canvas.Brush.Color:=c;
  end;
end;

procedure ListView_DrawImage(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean; ImageList: TCustomImageList; ImageIndex: Integer; BgColor: TColor; DrawText: Boolean = False; Text: string = '');
var
  Rect, MyRect: TRect;
  p: TPoint;
  i,x: Integer;
  c: TColor;
  Bitmap: TBitmap;
begin
  with (Sender as TListView) do begin
    p:=Item.Position;
    x:=0;
    for i:=0 to SubItem-1 do
      x:=x+Column[i].Width;
    Rect.Top:=p.y;
    Rect.Left:=p.x+x;
    Rect.Bottom:=Rect.Top+16;
    Rect.Right:=Rect.Left+Column[SubItem].Width;

    c:=Canvas.Brush.Color;
    if (RowSelect or (SubItem=0)) then
      if (cdsFocused in State) then begin
        Canvas.Brush.Color:=clHighlight;
        if (cdsHot in State) then
          Canvas.Brush.Color:=clHotlight;
        if not Assigned(Selected) then
          Canvas.Brush.Color:=c;
      end else
        Canvas.Brush.Color:=c;

    {$IFDEF THEMESUPPORT}
    if not UseThemes then
    {$ENDIF}
      Canvas.Rectangle(Rect);

    MyRect.Top:={((Rect.Bottom - Rect.Top - 16) div 2) + }Rect.Top+1;
    MyRect.Left:={((Rect.Right - Rect.Left - 11) div 2) +} Rect.Left+1;
    MyRect.Bottom:=MyRect.Top + 15;
    MyRect.Right:=MyRect.Left + 14;

    if Canvas.Brush.Color<>BgColor then
      Canvas.Pen.Color:=BgColor
    else
      Canvas.Pen.Color:=clBlack;

    if Item.Selected then
      Canvas.CopyMode:=cmSrcCopy
    else
      Canvas.CopyMode:=cmSrcCopy;

    Bitmap:=TBitmap.Create;
    try
      ImageList.GetBitmap(ImageIndex,Bitmap);
      Canvas.StretchDraw(MyRect,Bitmap);
    finally
      Bitmap.Free;
    end;
    if DrawText then
      Canvas.TextOut(MyRect.Left+20,Rect.Top+2,Text);

    Canvas.Brush.Color:=c;
  end;
end;

procedure ListView_ExportToCSV(AList: TListView; AFilename: string; AIncludeHeader: boolean = True);
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    ListView_ExportToCSV(AList,sl,AIncludeHeader);
    sl.SaveToFile(AFilename);
  finally
    sl.Free;
  end;
end;

procedure ListView_ExportToCSV(AList: TListView; AStrings: TStrings; AIncludeHeader: boolean = True);
var
  i,j: Integer;
  s,f: string;
begin
  try
    s:='';
    if AIncludeHeader then begin
      for i:=0 to AList.Columns.Count-1 do
        s:=s+AList.Column[i].Caption+';';
      Setlength(s,Length(s)-1);
      AStrings.Add(s);
    end;
    for j:=0 to AList.Items.Count-1 do begin
      s:='';
      for i:=0 to AList.Columns.Count-1 do begin
        if i=0 then
          f:=AList.Items[j].Caption
        else
          f:=AList.Items[j].SubItems[i-1];
        {if AList.Column[i].Alignment<>taRightJustify then
          f:='"'+f+'"';}
        s:=s+f+';'
      end;
      Setlength(s,Length(s)-1);
      AStrings.Add(s);
    end;
  finally

  end;
end;

{$IFNDEF FPC}
procedure ListView_Print;
var
  pWidth,pHeight,i: Integer;
  v,h: double;
  CurItem,iColumnCount: Integer;
  aCols: array of Integer;
  iTotColsWidth,iInnerWidth,TopMarg,LinesOnPage,CurLine,TxtHeight,CurCol: Integer;
  CurRect: TRect;
  CurStr,s: string;
  CurLeft,NumPages: Integer;
begin
  iColumnCount:=AList.Columns.Count;
  SetLength(aCols, iColumnCount + 1);
  Printer.Title:=ATitle;
  Printer.Copies:=1;
  Printer.Orientation:=poPortrait;
  Printer.BeginDoc;
  pHeight:=Printer.PageHeight;
  pWidth:=Printer.PageWidth;

  v:=(pHeight + (2 * GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY))) / (29.7 * 0.95);
  //0.95 is a strange correction factor on the clients printer
  h:=(pWidth + (2 * GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX))) / 21;

  // calculate total width
  iTotColsWidth:=0;
  for i:=0 to iColumnCount - 1 do
    iTotColsWidth:=iTotColsWidth + AList.Columns[i].Width;

  // calculate space between lMargin and rMargin
  aCols[0]:=Round(1.5 * h); //left margin ?
  aCols[iColumnCount + 0]:=pWidth - Round(1.5 * h); //rigth margin ?
  iInnerWidth:=aCols[iColumnCount + 0] - aCols[0]; // space between margins ?

  //calculate start of each column
  for i:=0 to iColumnCount - 1 do
    aCols[i + 1]:=aCols[i] + Round(AList.Columns[i].Width / iTotColsWidth * iInnerWidth);
  TopMarg:=Round(2 * v);
  with Printer.Canvas do begin
    Font.Size:=8;
    Font.Style:=[];
    Font.Name:='Tahoma';
    Font.Color:=RGB(0, 0, 0);
    TxtHeight:=Printer.Canvas.TextHeight('W');
    LinesOnPage:=Round((PHeight - (4 * v)) / TxtHeight);
    NumPages:=1;

    // gather number of pages to print
    while (NumPages * LinesOnPage) < AList.Items.Count do
      inc(NumPages);
    // start
    CurLine:=0;
    for CurItem:=0 to AList.Items.Count - 1 do begin
      if (CurLine > LinesOnPage) or (CurLine = 0) then begin
        if (CurLine > LinesOnPage) then
          Printer.NewPage;
        CurLine:=1;
        if Printer.PageNumber = NumPages then begin
          MoveTo(aCols[1], topMarg);
          for i:=1 to iColumnCount - 1 do begin
            LineTo(aCols[i], TopMarg + (TxtHeight * (AList.Items.Count - CurItem + 2)));
            MoveTo(aCols[i + 1], topMarg);
          end;
        end else begin
          // draw vertical lines between data
          for i:=1 to iColumnCount - 1 do
            if AList.Columns[i-1].Width>0 then begin
              MoveTo(aCols[i], topMarg);
              LineTo(aCols[i], TopMarg + (TxtHeight * (LinesOnPage + 1)));
            end;
        end;

        Font.Size:=8;
        Font.Style:=[fsBold];
        // print column headers
        for i:=0 to iColumnCount - 1 do begin
          if AList.Columns[i].Width>0 then
          TextRect(Rect(aCols[i] + Round(0.1 * h), TopMarg - Round(0.1 * v), aCols[i + 1] - Round(0.1 * h)
            , TopMarg + TxtHeight - Round(0.1 * v)), ((aCols[i + 1] - aCols[i]) div 2) +
            aCols[i] - (TextWidth(AList.Columns.Items[i].Caption) div 2),
            TopMarg - Round(0.1 * v), AList.Columns.Items[i].Caption);
          //showmessage('print kolom: '+IntToStr(i));
        end;

        // draw horizontal line beneath column headers
        MoveTo(aCols[0] - Round(0.1 * h), TopMarg + TxtHeight - Round(0.05 * v));
        LineTo(aCols[iColumnCount] + Round(0.1 * h), TopMarg + TxtHeight - Round(0.05 * v));

        // print date, page number, title, subtitle, footer
        Font.Size:=10;
        Font.Style:=[];

        TextOut((pWidth-iInnerWidth) div 2+(iInnerWidth-TextWidth(AFooter)) div 2, pHeight - Round(v), AFooter);
        TextOut((pWidth-iInnerWidth) div 2, pHeight - Round(v), DateTimeToStr(Now));
        s:=Format('%d / %d',[Printer.PageNumber,NumPages]);
        TextOut(pWidth-TextWidth(s)-(pWidth-iInnerWidth) div 2, pHeight - Round(v), s);

        TextOut((pWidth-iInnerWidth) div 2, Round(v), ATitle);
        TextOut(pWidth-TextWidth(ASubtitle)-(pWidth-iInnerWidth) div 2, Round(v), ASubtitle);

        Font.Size:=8;
        Font.Style:=[];
      end;

      CurRect.Top:=TopMarg + (CurLine * TxtHeight);
      CurRect.Bottom:=TopMarg + ((CurLine + 1) * TxtHeight);

      // print contents of Listview
      for CurCol:=-1 to iColumnCount - 2 do
        if AList.Columns[CurCol+1].Width>0 then begin
        CurRect.Left:=aCols[CurCol + 1] + Round(0.1 * h);
        CurRect.Right:=aCols[CurCol + 2] - Round(0.1 * h);
        try
          if CurCol = -1 then
            CurStr:=AList.Items[CurItem].Caption
          else
            CurStr:=AList.Items[CurItem].SubItems[CurCol];
        except
          CurStr:='';
        end;
        CurLeft:=CurRect.Left; // align left side
        // write string in TextRect
        TextRect(CurRect, CurLeft, CurRect.Top, CurStr);
      end;
      Inc(CurLine);
    end;
  end;
  Printer.EndDoc;
  Finalize(aCols);
end;
{$ENDIF}

function Form_Show;
var
  i: integer;
begin
  Result:=False;
  for i:=0 to Screen.FormCount-1 do
    if Screen.Forms[i].ClassType=Sender then begin
      Result:=True;
      Screen.Forms[i].Show;
      Break;
    end;
end;

procedure Form_SetInvisible;
var
  acontrol :tcontrol;
  i,margin,x,y,ctlx,ctly :integer;
begin
  with Sender do begin
    margin:=(width-clientwidth ) div 2;
    fullrgn:=createrectrgn(0,0,width,height);
    x:=margin;
    y:=height-clientheight-margin;
    clientrgn:=createrectrgn(x,y,x+clientwidth,y+clientheight);
    combinergn(fullrgn,fullrgn,clientrgn,RGN_DIFF);
    for i:=0 to controlcount-1 do begin
      acontrol:=controls[i];
      if (acontrol is twincontrol) or (acontrol is tgraphiccontrol) then
        with acontrol do begin
          if visible then begin
            ctlx:=x+left;
            ctly:=y+top;
            ctlrgn:=createrectrgn(ctlx,ctly,ctlx+width,ctly+height);
            combinergn(fullrgn,fullrgn,ctlrgn,RGN_OR);
        end;
      end;
    end;
    setwindowrgn(handle,fullrgn,true);
  end;
  //DeleteObject(FullRgn);
end;

procedure Form_SetVisible;
begin
  with Sender do begin
    fullrgn:=createrectrgn(0,0,width,height);
    combinergn(fullrgn,fullrgn,fullrgn,RGN_COPY);
    setwindowrgn(handle,fullrgn,true);
    //DeleteObject(FullRgn);
  end;
end;

procedure Form_HideCaption;
var
  FDiff: integer;
begin
  if Sender.BorderStyle in [bsDialog, bsSingle, bsSizeable] then
    FDiff:=GetSystemMetrics(SM_CYCAPTION)
  else if Sender.BorderStyle in [bsToolWindow, bsSizeToolWin] then
    FDiff:=GetSystemMetrics(SM_CYSMCAPTION)
  else
    FDiff:=0;  
  SetWindowLong(Sender.Handle,GWL_STYLE,GetWindowLong(Sender.Handle,GWL_Style) and not WS_Caption);
  Sender.Height:=Sender.Height-FDiff;
end;

procedure Form_ShowCaption;
var
  FDiff: integer;
begin
  FDiff:=GetSystemMetrics(SM_CYCAPTION);
  SetWindowLong(Sender.Handle,GWL_STYLE,GetWindowLong(Sender.Handle,GWL_Style)+WS_Caption);
  Sender.Height:=Sender.Height+FDiff;
end;

function Form_IsCaptionVisible;
begin
  {$IFDEF RAD7PLUS}
  Result:=GetWindowLongPtr(Sender.Handle,GWL_STYLE) and WS_CAPTION>0;
  {$ELSE}
  Result:=GetWindowLong(Sender.Handle,GWL_STYLE) and WS_CAPTION>0;
  {$ENDIF}
end;

procedure Form_Move;
begin
  ReleaseCapture;
  Sender.Perform(WM_SYSCOMMAND,$f012,0);
end;

function Tree_FindNode(AParent: TTreeNode; AText: string): TTreeNode;
var
  n: TTreeNode;
begin
  Result:=nil;
  n:=AParent.getFirstChild;
  while Assigned(n) do begin
    if SameText(AText,n.Text) then begin
      Result:=n;
      Break;
    end;
    n:=n.getNextSibling;
  end;
end;


function Tree_FindNode(Sender: TTreeView; AText: string): TTreeNode;
var
  i: integer;
begin
  Result:=nil;
  AText:=UpperCase(Atext);
  for i:=0 to Sender.Items.Count-1 do
    if UpperCase(Sender.Items[i].Text)=AText then begin
      Result:=Sender.Items[i];
      Break;
    end;
end;

function Tree_FindNodeByPath;
var
  p: Integer;
  s: string;
begin
  Result:=nil;
  p:=Pos('\',AText);
  while p>0 do begin
    s:=Copy(AText,1,p-1);
    Delete(AText,1,p);
    if not Assigned(Result) then
      Result:=Tree_FindNode(Sender,s)
    else begin
      Result:=Result.getFirstChild;
      while Assigned(Result) do begin
        if SameText(Result.Text,s) then
          Break;
        Result:=Result.GetNextSibling;
      end;
    end;
    p:=Pos('\',AText);
    if not Assigned(Result) then
      p:=0;
  end;
end;

function Tree_GetNodePath;
begin
  Result:='';
  while Assigned(ANode) do begin
    try
      Result:=Format('%s\%s',[ANode.Text,Result]);
      ANode:=ANode.Parent;
    except
      Break;
    end;
  end;
end;

function Tree_CreateNodeByPath;
var
  p: Integer;
  s: string;
  r: TTreeNode;
begin
  Result:=nil;
  r:=ARoot;
  p:=Pos('\',AText);
  while p>0 do begin
    s:=Copy(AText,1,p-1);
    Delete(AText,1,p);
    if not Assigned(Result) then
      Result:=Tree_FindNode(Sender,s)
    else begin
      Result:=Result.getFirstChild;
      while Assigned(Result) do begin
        if SameText(Result.Text,s) then
          Break;
        Result:=Result.GetNextSibling;
      end;
    end;
    p:=Pos('\',AText);
    if not Assigned(Result) then begin
      Result:=Sender.Items.AddChild(r,s);
      Result.ImageIndex:=AImageIndex;
      Result.SelectedIndex:=Result.ImageIndex;
      r:=Result;
    end else
      r:=Result;
  end;
end;

procedure Tree_ExportToText;
var
  i: Integer;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    for i:=0 to Sender.Items.Count-1 do
      sl.Add(StringOfChar(' ',Sender.Items[i].Level*2)+Sender.Items[i].Text);
    sl.SaveToFile(AFilename);  
  finally
    sl.Free;
  end;
end;

procedure Stat_SetText;
begin
  Sender.panels[aindex].text:=atext;
  Sender.panels[aindex].width:=Sender.canvas.textwidth(atext)+10;
  Sender.Update;
end;

{
procedure DBGrid_DrawBitmaps;
var
  MyRect: TRect;
  c: TColor;
begin
  c:=Canvas.Pen.Color;
  Canvas.Pen.Color:=Color;
  Canvas.Rectangle(Rect);
  Canvas.Pen.Color:=c;
  MyRect.Top:=((Rect.Bottom - Rect.Top - 17) div 2) + Rect.Top;
  MyRect.Left:=Rect.Left;
  MyRect.Bottom:=MyRect.Top + 16;
  MyRect.Right:=MyRect.Left + 16;
  if Selected then
    Canvas.CopyMode:=cmSrcCopy
  else
    Canvas.CopyMode:=cmSrcCopy;
  Canvas.StretchDraw(MyRect,Bitmap);
  if DrawText then
    Canvas.TextOut(MyRect.Left+20,Rect.Top+2,Field.AsString);
end;

procedure DBGrid_DrawCheckBoxes;
var
  MyRect: TRect;
  c: TColor;
begin
  c:=Canvas.Pen.Color;
  Canvas.Pen.Color:=Color;
  Canvas.Rectangle(Rect);
  Canvas.Pen.Color:=c;
  MyRect.Top:=((Rect.Bottom - Rect.Top - 11) div 2) + Rect.Top;
  MyRect.Left:=((Rect.Right - Rect.Left - 11) div 2) + Rect.Left;
  MyRect.Bottom:=MyRect.Top + 10;
  MyRect.Right:=MyRect.Left + 10;
  if Selected then
    Canvas.Pen.Color:=clYellow
  else
    Canvas.Pen.Color:=clBlack;
  Canvas.Polyline([
    Point(MyRect.Left, MyRect.Top), Point(MyRect.Right, MyRect.Top),
    Point(MyRect.Right, MyRect.Bottom), Point(MyRect.Left, MyRect.Bottom),
    Point(MyRect.Left, MyRect.Top)]);
  if fIELD.Value=TrueValue then begin
    Canvas.MoveTo(MyRect.Left + 2, MyRect.Top + 4);
    Canvas.LineTo(MyRect.Left + 2, MyRect.Top + 7);
    Canvas.MoveTo(MyRect.Left + 3, MyRect.Top + 5);
    Canvas.LineTo(MyRect.Left + 3, MyRect.Top + 8);
    Canvas.MoveTo(MyRect.Left + 4, MyRect.Top + 6);
    Canvas.LineTo(MyRect.Left + 4, MyRect.Top + 9);
    Canvas.MoveTo(MyRect.Left + 5, MyRect.Top + 5);
    Canvas.LineTo(MyRect.Left + 5, MyRect.Top + 8);
    Canvas.MoveTo(MyRect.Left + 6, MyRect.Top + 4);
    Canvas.LineTo(MyRect.Left + 6, MyRect.Top + 7);
    Canvas.MoveTo(MyRect.Left + 7, MyRect.Top + 3);
    Canvas.LineTo(MyRect.Left + 7, MyRect.Top + 6);
    Canvas.MoveTo(MyRect.Left + 8, MyRect.Top + 2);
    Canvas.LineTo(MyRect.Left + 8, MyRect.Top + 5);
  end;
end;
}

procedure StringGrid_Clear(grid: TStringGrid);
var
  i,j: integer;
begin
  for i:=0 to grid.ColCount-1 do
    for j:=0 to grid.RowCount-1 do
      grid.Cells[i,j]:='';
  grid.ColCount:=0;
  grid.RowCount:=0;
end;

procedure StringGrid_SelectCell(grid: TStringGrid; ACol, ARow: integer);
var
  s: TGridRect;
begin
  s.Left:=ACol;
  s.Top:=ARow;
  s.Right:=ACol;
  s.Bottom:=ARow;
  grid.Selection:=s;
end;

procedure StringGrid_ExportToCSV(grid: TStringGrid; FileName: string);
var
  sl: TStringList;
  i,j: Integer;
  s: string;
const
  Delimiter = ';';
begin
  sl:=TStringList.Create;
  try
    s:='';
    for i:=0 to grid.RowCount-1 do begin
      s:='';
      for j:=0 to grid.ColCount-1 do
        s:=s+grid.Cells[j,i]+Delimiter;
      sl.Add(s);
    end;
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

procedure StringGrid_ImportCSV(grid: TStringGrid; FileName: string);
var
  sl: TStringList;
  i,j,n: Integer;
  s: string;
const
  Delimiter = ';';
begin
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    if sl.Count>0  then begin
      s:='';
      n:=GetTokenCount(sl[0],Delimiter);
      for i:=0 to sl.Count-1 do
        for j:=0 to n-1 do
          grid.Cells[j,i]:=GetToken(sl[i],j+1,Delimiter);
    end;
  finally
    sl.Free;
  end;
end;

{$IFNDEF FPC}
procedure ImageList_ConvertToHighColor(ImageList: TImageList);
var
  IL: TImageList;
begin
  IL:=TImageList.Create(nil);
  IL.Assign(ImageList);
  with ImageList do
    Handle:=ImageList_Create(Width,Height,ILC_COLOR16 or ILC_MASK,Count,AllocBy);
  ImageList.Assign(IL);
  IL.Free;
end;
{$ENDIF}

function Edit_GetValue(AEdit: TCustomEdit): Double;
begin
  try
    Result:=StrToFloat(AEdit.Text);
  except
    Result:=0;
  end;
end;

procedure Edit_SetValue(AEdit: TCustomEdit; AValue: Double);
begin
  AEdit.Text:=FloatToStr(AValue);
end;

procedure PageControl_HideAllTabs(pc: TPageControl);
var
  i: Integer;
begin
  for i:=0 to pc.PageCount-1 do
    pc.Pages[i].TabVisible:=False;
  pc.ActivePageIndex:=0;
end;

procedure ComboBox_AddHistory(cb: TComboBox; AText: string = '');
begin
  if AText='' then
    AText:=cb.Text;
  if AText='' then
    Exit;
  if cb.Items.IndexOf(AText)=-1 then
    cb.Items.Add(AText);
end;

function StretchRect(AStart,ADest: TRect; ACenterX: Boolean = True; ACenterY: Boolean = True; APreserveRatio: Boolean = True; AMode: TStretchMode = smAny): TRect;
var
  h, w: integer;
  stx, sty, st: double;
begin
  if IsRectEmpty(AStart) or IsRectEmpty(ADest) then begin
    Result:=Bounds(0,0,0,0);
    Exit;
  end;
  if (AMode=smAny) or
     ((AMode=smOnlySmaller) and ((RectHeight(AStart)>RectHeight(ADest)) or (RectWidth(AStart)>RectWidth(ADest)))) or
     ((AMode=smOnlyBigger) and ((RectHeight(AStart)<RectHeight(ADest)) or (RectWidth(AStart)<RectWidth(ADest)))) then begin
    if APreserveRatio then begin
      stx:=RectWidth(AStart)/RectWidth(ADest);
      sty:=RectHeight(AStart)/RectHeight(ADest);
      if stx<sty then
        st:=sty
      else
        st:=stx;
      w:=round(RectWidth(AStart)/st);
      h:=round(RectHeight(AStart)/st);
      Result:=Bounds(ADest.Left, ADest.Top, w, h);
      if ACenterX then
        Result:=Bounds(ADest.Left+(RectWidth(ADest)-w) div 2, ADest.Top, w, h);
      if ACenterY then
        Result:=Bounds(Result.Left, ADest.Top+(RectHeight(ADest)-h) div 2, w, h);
    end else
      Result:=ADest;
  end else begin
    Result:=AStart;
    w:=RectWidth(Result);
    h:=RectHeight(Result);
    if ACenterX then
      Result:=Bounds(ADest.Left+(RectWidth(ADest)-w) div 2, ADest.Top, w, h);
    if ACenterY then
      Result:=Bounds(Result.Left, ADest.Top+(RectHeight(ADest)-h) div 2, w, h);
  end;
end;

procedure ConvertICO2BMP(Icon: TIcon; TransColor: TColor; Bitmap: TBitmap);
var
  i,j: integer;
  IconInfo: TIconInfo;
  ANDmask: TBitmap;
  OldMaskColor: TColor;
  NewMaskColor: TColor;
begin
  NewMaskColor:=clNone;
  ANDmask:=TBitmap.Create;
  try
    GetIconInfo(Icon.Handle,IconInfo);
    ANDmask.Handle:=IconInfo.hbmMask;
    Bitmap.Width:=ANDmask.Width;
    Bitmap.Height:=ANDmask.Height + 1;
    Bitmap.Canvas.Draw(0,0,Icon);

    Bitmap.Canvas.Pen.Color:=TransColor;
    for j:=0 to ANDmask.Height - 1 do  begin
      OldMaskColor:=clRed;
      Bitmap.Canvas.MoveTo(0,j);
      for i:=0 to ANDmask.Width - 1 do begin
        NewMaskColor:=GetPixel(ANDmask.Canvas.Handle,i,j);
        if NewMaskColor <> OldMaskColor then  begin
          OldMaskColor:=NewMaskColor;
          if NewMaskColor = clWhite then
            Bitmap.Canvas.MoveTo(i,j)
          else
            Bitmap.Canvas.LineTo(i,j);
        end;
      end;
      if NewMaskColor = clWhite then
        Bitmap.Canvas.LineTo(ANDmask.Width,j);
    end;
    Bitmap.Canvas.MoveTo(0,Bitmap.Height - 1);
    Bitmap.Canvas.LineTo(Bitmap.Width,Bitmap.Height - 1);
  finally
    ANDmask.Free;
  end;
end;

procedure SetMainMenu(AMainMenu: TMainMenu; AEnabled: Boolean);
var
  i: Integer;
begin
  for i:=0 to AMainMenu.Items.Count-1 do
    AMainMenu.Items[i].Enabled:=AEnabled;
end;

end.
