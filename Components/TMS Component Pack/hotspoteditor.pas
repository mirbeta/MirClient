{*************************************************************************}
{ THotSpotImage design time editor                                        }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2002 - 2012                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit HotSpotEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Math, Buttons, ToolWin, ComCtrls, ImgList,
  Menus, jpeg, ExtDlgs, HotSpotImage, LineLibrary, Spin, Clipbrd;

const
  BordRectSz = 2;
type
  TStatus = (stNormal, stDrawing, stDrawPoly, stMovePoint, stMoveLine, stMove,
             stScaleEllipse, stWand);

  TBool2DArray = array of array of boolean;
  TRGBTripleRow = array[0..30000]of trgbtriple;
  pRGBTripleRow = ^TRGBTripleRow;

  TfrmHSIEditor = class(TForm)
    ToolBar1: TToolBar;
    btRect: TSpeedButton;
    btNormal: TSpeedButton;
    pnImage: TPanel;
    pmHotSpot: TPopupMenu;
    miDelete: TMenuItem;
    OPD: TOpenPictureDialog;
    pnButtons: TPanel;
    btOk: TButton;
    btCancel: TButton;
    pmClickImage: TPopupMenu;
    miClearClickImage: TMenuItem;
    pmHoverImage: TPopupMenu;
    miClearHoverImage: TMenuItem;
    N1: TMenuItem;
    miSaveHSImage: TMenuItem;
    SPD: TSavePictureDialog;
    Splitter: TSplitter;
    pnContainer: TPanel;
    pnProperties: TPanel;
    lblHint: TLabel;
    pnTitle: TPanel;
    txtHint: TMemo;
    lbHover: TLabel;
    pnHoverImage: TPanel;
    imgHover: TImage;
    btLoadHover: TButton;
    lblClick: TLabel;
    pnClickImage: TPanel;
    imgClick: TImage;
    btLoadClick: TButton;
    pnBackground: TPanel;
    btBackImage: TButton;
    btEllipse: TSpeedButton;
    lblName: TLabel;
    lblID: TLabel;
    edtName: TEdit;
    btDelete: TSpeedButton;
    pnLoadBack: TPanel;
    edtID: TEdit;
    btPoly: TSpeedButton;
    BevelSep: TBevel;
    btAddPoint: TSpeedButton;
    btDelPoint: TSpeedButton;
    lbPozx: TLabel;
    lbPozy: TLabel;
    btDelLine: TSpeedButton;
    spAngle: TSpinEdit;
    lbAngle: TLabel;
    btCopyToClipBoard: TSpeedButton;
    lbnX: TLabel;
    lbnY: TLabel;
    ckClip: TCheckBox;
    btWand: TSpeedButton;
    trTol: TTrackBar;
    lbWTolerance: TLabel;
    trDensity: TTrackBar;
    lbWDensity: TLabel;
    lbNPoints: TLabel;
    ToolButton1: TToolButton;
    btZoomIn: TSpeedButton;
    btZoomOut: TSpeedButton;
    sbPicture: TScrollBox;
    PB: TPaintBox;
    btZoomRST: TSpeedButton;
    seZoomRatio: TSpinEdit;
    lbZoomRatio: TLabel;
    PageControl1: TPageControl;
    ts_Click: TTabSheet;
    ts_Hover: TTabSheet;
    ts_Seletced: TTabSheet;
    ts_Blink: TTabSheet;
    Btn_Selected: TButton;
    Pn_Selected: TPanel;
    Img_Selected: TImage;
    lbl_Selected: TLabel;
    Btn_Blink: TButton;
    Pn_Blink: TPanel;
    Img_Blink: TImage;
    lbl_Blink: TLabel;
    lbl_ClickImgIndex: TLabel;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    Pn_ClickColor: TPanel;
    cmb_ClickImgIndex: TComboBox;
    Label2: TLabel;
    cmb_HoverImgIndex: TComboBox;
    Label3: TLabel;
    Pn_HoverColor: TPanel;
    Label4: TLabel;
    cmb_SelectImgIndex: TComboBox;
    Label5: TLabel;
    Pn_SelectedColor: TPanel;
    Label6: TLabel;
    cmb_BlinkImgIndex: TComboBox;
    Label7: TLabel;
    Pn_BlinkColor: TPanel;
    chk_Blink: TCheckBox;
    pm_SelectedImage: TPopupMenu;
    pm_BlinkImage: TPopupMenu;
    mi_CLearSelectImage: TMenuItem;
    mi_ClearBlinkImage: TMenuItem;
    se_OffsetX: TSpinEdit;
    Label8: TLabel;
    se_OffsetY: TSpinEdit;
    Label9: TLabel;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    btClearClick: TButton;
    btClearHover: TButton;
    btClearSelected: TButton;
    btClearBlink: TButton;
    bt_ClearClickColor: TButton;
    bt_ClearHoverColor: TButton;
    bt_ClearSelectColor: TButton;
    bt_ClearBlinkColor: TButton;
    ckSelectable: TCheckBox;
    Shape_ClickColor: TShape;
    Shape_HoverColor: TShape;
    Shape_SelectedColor: TShape;
    Shape_BlinkColor: TShape;
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure btLoadClickClick(Sender: TObject);
    procedure btLoadHoverClick(Sender: TObject);
    procedure btRectClick(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure miClearClickImageClick(Sender: TObject);
    procedure miClearHoverImageClick(Sender: TObject);
    procedure miSaveHSImageClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btBackImageClick(Sender: TObject);
    procedure btEllipseClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btDeleteClick(Sender: TObject);
    procedure edtIDKeyPress(Sender: TObject; var Key: Char);
    procedure edtNameMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtIDMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btPolyClick(Sender: TObject);
    procedure spAngleChange(Sender: TObject);
    procedure btCopyToClipBoardClick(Sender: TObject);
    procedure btNormalClick(Sender: TObject);
    procedure ckClipClick(Sender: TObject);
    procedure btWandClick(Sender: TObject);
    procedure trTolChange(Sender: TObject);
    procedure btZoomInClick(Sender: TObject);
    procedure btZoomOutClick(Sender: TObject);
    procedure btZoomRSTClick(Sender: TObject);
    procedure Pn_ClickColorClick(Sender: TObject);
    procedure Btn_SelectedClick(Sender: TObject);
    procedure Btn_BlinkClick(Sender: TObject);
    procedure chk_BlinkClick(Sender: TObject);
    procedure mi_CLearSelectImageClick(Sender: TObject);
    procedure mi_ClearBlinkImageClick(Sender: TObject);
    procedure se_OffsetXChange(Sender: TObject);
    procedure se_OffsetYChange(Sender: TObject);
    procedure cmb_ClickImgIndexChange(Sender: TObject);
    procedure cmb_HoverImgIndexChange(Sender: TObject);
    procedure cmb_SelectImgIndexChange(Sender: TObject);
    procedure cmb_BlinkImgIndexChange(Sender: TObject);
    procedure cmb_BlinkImgIndexDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure Pn_HoverColorClick(Sender: TObject);
    procedure Pn_SelectedColorClick(Sender: TObject);
    procedure Pn_BlinkColorClick(Sender: TObject);
    procedure btDelLineClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure btClearClickClick(Sender: TObject);
    procedure btClearHoverClick(Sender: TObject);
    procedure btClearSelectedClick(Sender: TObject);
    procedure btClearBlinkClick(Sender: TObject);
    procedure bt_ClearClickColorClick(Sender: TObject);
    procedure bt_ClearHoverColorClick(Sender: TObject);
    procedure bt_ClearSelectColorClick(Sender: TObject);
    procedure bt_ClearBlinkColorClick(Sender: TObject);
    procedure ckSelectableClick(Sender: TObject);
  private
    function getHoverBorderItem: integer;
    procedure SetHoverBorderItem(const Value: integer);
    procedure DrawHotSpotImage(HotSpot: THotSpot; Canvas: TCanvas);  // draw image using ImageIndex
  private
    FBorderPoly: TBorderPoly;
    FHoverPos: THoverPosition;
    FHoverBorderItem,FHoverItem, FSelectedItem: Integer;
    PStart, PCurrent: TPoint;
    FStatus: TStatus;
    CurrentDelta:integer;
    procedure disablePolyTools;
    procedure enablePolyTools;
    procedure CalcWandPoly(var bitmap1:TBitmap;var mask: TBool2DArray);
    procedure MagicWand(X,Y:Integer;tolerance:Real);
    procedure disableWandTools;
    procedure enableWandTools;
    function RestrictX(X: Integer): Integer;
    function RestrictY(Y: Integer): Integer;
    procedure SetSelectedItem(const Value: Integer);
    Function  GetSelectedItem:integer;
    function SavePoly:Boolean;
    procedure lProperties(doEnabled: Boolean);
    procedure SwapHotSpots;
    function GetImageIndex(cmb: TComboBox): Integer;
    procedure SetImageIndex(cmb: TComboBox; ImageIndex: Integer);
    procedure LoadAllImageComboboxes;
    property HoverBorderItem:integer read getHoverBorderItem write SetHoverBorderItem;
  public
    procedure LoadSettings(inifile:TFileName);
    procedure SaveSettings(inifile:TFileName);
    property SelectedItem: Integer read GetSelectedItem write SetSelectedItem;
  end;

var
  frmHSIEditor: TfrmHSIEditor;
  FHotSpots_org: THotSpots;
  FHotSpots: THotSpots;
  ImageList: TCustomImageList;
  HotSpotsChanged:boolean;
  FPicture: TPicture;
  tmPoints: TPoints;
  FPoint1,FPoint2: Integer;
  svMove: TPoint;

//============================================================ IMPLEMENTATION ==
implementation

uses
  INIFiles;

{$R *.DFM}

//------------------------------------------------------------------------------

var
  fillx:array[0..80000]of integer;
  filly:array[0..80000]of integer;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.CalcWandPoly(var bitmap1:tbitmap;var mask:tbool2darray);
var
  i,j,xi,yi,xc,yc,nx,ny: Integer;
  modified: Boolean;
  nPoints: Integer;
  tt,modifiable: array of boolean;
  a,c,Distance: Real;
  odir,dir: Integer;

//------------------------------------------------------------------------------
  function newx(x,dir:Integer):Integer;//calculate new x in the direction specified
  begin
    case (dir mod 8) of
    0:Result := x;                    //N
    1:Result := x + 1;                //NE
    2:Result := x + 1;                //E
    3:Result := x + 1;                //SE
    4:Result := x;                    //S
    5:Result := x - 1;                //SW
    6:Result := x - 1;                //W
    7:Result := x - 1;                //NW
    else
      Result := 0; //to stop compiler warnings
    end;
  end;

//------------------------------------------------------------------------------
  function newy(y,dir:Integer):Integer;
  begin
    case (dir mod 8) of
    0:Result := y - 1;                //N
    1:Result := y - 1;		      //NE
    2:Result := y;		      //E
    3:Result := y + 1;		      //SE
    4:Result := y + 1;		      //S
    5:Result := y + 1;		      //SW
    6:Result := y;		      //W
    7:Result := y - 1;		      //NW
    else
      Result := 0; //to stop compiler warnings
    end;
  end;

//------------------------------------------------------------------------------
begin
  nPoints := 0;
  tmPoints := nil;
  SetLength(tmPoints,20);//grow 20 points at a time
  SetLength(modifiable,20);

  xi := -1;
  yi := -1;

  //find the first point of the region
  for j := 0 to bitmap1.Height - 1 do
  begin
    for i := 0 to bitmap1.Width - 1 do
      if mask[i,j] then
      begin
        xi := i;
        yi := j;
        Break;
      end;
    if mask[i,j] then Break;
  end;

  //if there is no point then Exit
  if (xi < 0) or (yi < 0) then
  begin
    tmPoints := nil;
    Exit;
  end;

  xc := xi;
  yc := yi;
  dir := 8; //the first direction is North, orthogonal with current direction
            //(which is East since we were scanning left to right)

  odir := -1;
  j := 0; //"fire escape" variable

  repeat
    inc(j);
    for i := 0 to 7 do //for all 8 directions
    begin
      nx := newx(xc,dir);
      ny := newy(yc,dir);
      try
        if (nx >= bitmap1.Width) or (ny >= bitmap1.Height) or (nx < 0) or (ny < 0) then
          Continue;
        if mask[nx,ny] then //if it found a point then add it
        begin
          //if the direction changed (if not, it is useless to add another point)
          if (odir mod 8) <> (dir mod 8) then
          begin
            if nPoints >= Length(tmPoints) then
            begin
              SetLength(tmPoints,nPoints + 20);
              SetLength(modifiable,nPoints + 20);
            end;
            tmPoints[nPoints].x := xc;
            tmPoints[nPoints].y := yc;

            bitmap1.Canvas.Pixels[xc,yc] := clBlue;

            modifiable[nPoints] := True;
            inc(nPoints);
          end;
          //if it is a corner, mark it as non eraseable
          if ((8 + dir - odir) mod 8) > 1 then
            modifiable[nPoints-1] := False;
          xc := nx;
          yc := ny;
          odir := dir;
          //new direction is orthogonal on current direction (dir := dir - 3 + 1 -from finally)
          dir := ((dir + 5) mod 8) + 8;
          Break;
        end;
      finally
        inc(dir);
      end;
    end;
    if j > 1000000 then
      Break;  //in case of fire break window
  until (xc = xi) and (yc = yi);

  //simple algorithm to reduce the number of points:
  //repeat
  // for all points
  //  if the distance between i+1 and (i,i+3) line is smaller than Distance
  //  then delete i+1
  //until no point was deleted

  // for high accuracy, do not reduce points
  Setlength(tmPoints,nPoints);
  Setlength(tt,nPoints);

  Distance := 3 * ((trDensity.Max - trDensity.Position + 5) / trDensity.Max);

  repeat
    Setlength(tmPoints,nPoints);
    Setlength(tt,nPoints);
    Modified := False;

    for i := 0 to High(tmPoints) do
      tt[i] := True;

    for i := 0 to High(tmPoints)-2 do
    begin
      if (not tt[i])or(not modifiable[i+1]) then
        Continue;

      if Abs(tmPoints[i+2].x - tmPoints[i].x) < 0.0000001 then
      begin
        if Abs(tmPoints[i+1].x-tmPoints[i].x) < Distance then
        begin
          tt[i+1] := False;
          modified := True;
        end;
      end
      else
      begin
        a := -(tmPoints[i+2].y-tmPoints[i].y)/(tmPoints[i+2].x-tmPoints[i].x);
        c := -tmPoints[i].y-a*tmPoints[i].x;
        if((abs(a*tmPoints[i+1].x+tmPoints[i+1].y+c)/sqrt(a*a+1))<Distance) then
        begin
          tt[i+1] := False;
          modified := True;
        end;
      end;
    end;

    nPoints := 0;
    for i := 0 to High(tmPoints) do
    begin
      if tt[i] then
      begin
        if nPoints <> i then
          tmPoints[nPoints] := tmPoints[i];
        inc(nPoints);
      end;
    end;
  until modified = False;

  //showmessage('result nr. of polypoints: ' + inttostr(high(tmpoints)));

  a := PB.Width / FPicture.Width;
  c := PB.Height / FPicture.Height;

  if CurrentDelta <> 0 then
    for i := 0 to High(tmPoints) do
    begin
      tmPoints[i].x := tmPoints[i].x * a;
      tmPoints[i].y := tmPoints[i].y * c;
    end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.MagicWand(x,y: Integer;Tolerance: Real);
var
  nf: Integer;
  i,j,ir: Integer;
  xf,yf: Integer;
  bitmap1: TBitmap;
  mask: TBool2DArray;
  outermask: TBool2DArray;
  oldcolor: TColor;

  procedure grow_region;
  var
    pixr,pixg,pixb,oldr,oldg,oldb: Byte;
    pix: TColorref;
    jj: Byte;
    xr,yr: Longint;
    pp: prgbtriplerow;
  begin
    oldr := GetRValue(oldcolor);
    oldg := GetGValue(oldcolor);
    oldb := GetBValue(oldcolor);

    xr := xf + 1;
    yr := yf;

    for jj := 1 to 4 do
    begin
      //Growing the Region
      if jj = 1 then
      begin
        xr := xf + 1;
        yr := yf;
      end;
      if jj = 2 then
      begin
        xr := xf - 1;
        yr := yf;
      end;
      if jj = 3 then
      begin
        xr := xf;
        yr := yf + 1;
      end;
      if jj = 4 then
      begin
        xr := xf;
        yr := yf - 1;
      end;

      //if it's not outside the bitmap
      if ((xr < bitmap1.Width) and (jj = 1)) or ((xr >= 0) and (jj = 2))
        or ((yr < bitmap1.Height) and (jj = 3)) or ((yr >= 0)and (jj = 4)) then
      begin
        pp := bitmap1.Scanline[yr];
        pix := Rgb(pp[xr].rgbtred,pp[xr].rgbtgreen,pp[xr].rgbtblue);
        pixr := GetRValue(pix);
        pixg := GetGValue(pix);
        pixb := GetBValue(pix);
        // if the current pixel should be part of the region then add it to it
        if (not Mask[xr,yr]) and (Abs(pixr - oldr) <= Tolerance * 150) and (Abs(pixg-oldg) <= Tolerance * 150) and (Abs(pixb-oldb) <= tolerance*150)then
        begin
          mask[xr,yr] := true;

          nf := nf + 1;
          //add to queue
          fillx[nf] := xr;
          filly[nf] := yr;
        end;
      end;
    end;
  end;

//------------------------------------------------------------------------------
begin
  //Image initialization
  if (x > FPicture.Width + CurrentDelta) or (x < 0) or
   (y < 0) or (y > FPicture.Height + CurrentDelta) then
    Exit;

  if CurrentDelta <> 0 then
  begin
    x := Round(x * FPicture.Width / PB.Width);
    y := Round(y * FPicture.Height / PB.Height);
  end;

  bitmap1 := TBitmap.Create;
  try
    // copy picture to bitmap
    bitmap1.Width := FPicture.Width;
    bitmap1.Height := FPicture.Height;
    bitmap1.PixelFormat := pf24Bit;
    bitmap1.Canvas.Draw(0,0,FPicture.Graphic);

    // initialize boolean array
    SetLength(mask,bitmap1.Width + 5,bitmap1.Height + 5);
    SetLength(outermask,bitmap1.Width + 5,bitmap1.Height + 5);

    // start color
    oldcolor := bitmap1.Canvas.Pixels[x,y];
    nf := 1;
    ir := 1;
    fillx[nf] := x;
    filly[nf] := y;
    xf := x;
    yf := y;
    grow_region;

    while (nf > ir) do
    begin
      //extract point & grow_region(point)
      ir := ir + 1;
      xf := fillx[ir];
      yf := filly[ir];
      grow_region;
      if (nf > 75000)  then
      begin
        for i := 1 to nf-ir do
        begin
          fillx[i] := fillx[ir + i];
          filly[i] := filly[ir + i];
        end;
        nf := nf - ir ;
        ir := 0;
      end;
    end;

   for i := 0 to bitmap1.Width - 1 do
   begin
     for j := 0 to bitmap1.Height - 1 do
     begin
       if mask[i,j] then
       begin
         outermask[i,j] := mask[i,j];

         if (i > 0) then
           outermask[i - 1, j] := true;

         if (j > 0) then
           outermask[i, j - 1] := true;

         if (i < bitmap1.Width) then
           outermask[i + 1, j] := true;

         if (j < bitmap1.Height) then
           outermask[i, j + 1] := true;
       end;

     end;
   end;

    CalcWandPoly(bitmap1,outermask);
  finally
    bitmap1.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.DisablePolyTools;
begin
  btAddPoint.Enabled := False;
  btDelPoint.Enabled := False;
  btDelLine.Enabled := False;
  lbNPoints.Visible := False;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.EnablePolyTools;
begin
  btAddPoint.Enabled := True;
  btDelPoint.Enabled := True;
  btDelLine.Enabled := True;
  lbNPoints.Visible := True;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.DisableWandTools;
begin
  trTol.Visible := False;
  lbWTolerance.Visible := False;
  lbWDensity.Visible := False;
  trDensity.Visible := False;
  lbNPoints.Visible := False;
  btZoomIn.Enabled:=true;
  btZoomOut.Enabled:=true;
  btZoomRST.Enabled:=true;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.enableWandTools;
begin
  trTol.Visible := True;
  lbWTolerance.Visible := True;
  lbWDensity.Visible := True;
  trDensity.Visible := True;
  lbNPoints.Visible := True;
  btZoomIn.Enabled:=false;
  btZoomOut.Enabled:=false;
  btZoomRST.Enabled:=false;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.lProperties(doEnabled: Boolean);
var
  ll: TList;
  pp:Pointer;
  tc: TControl;
  i: Integer;
begin
  pnProperties.Enabled := doEnabled;
  ll := TList.Create;
  if ll = nil then
    Exit;
  pnProperties.GetTabOrderList(ll);
  for i:=0 to ll.Count-1 do
  begin
    pp:=ll[i];
    if pp=nil then continue;
    try
      tc:=pp;
      tc.Enabled:= doEnabled;
    except
      continue;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TfrmHSIEditor.SavePoly: boolean;
var
  tt:THotSpot;
begin
  if length(tmPoints)<3 then
  begin
    tmPoints := nil;
    Result := False;
    Exit;
  end;
  FStatus := stNormal;
  tt := FHotSpots.Add;
  tt.Clipped := True;
  tt.ShapeType := stPolygon;
  tt.PolyPoints := tmPoints;
  if btPoly.Down then
    btNormal.Down:= True;
  tmPoints := nil;
  PBPaint(Self);
  Result := True;
  HotSpotsChanged:=true;
end;

//------------------------------------------------------------------------------

procedure TfrmHSIEditor.LoadAllImageComboboxes;
var
  i: Integer;
begin
  cmb_ClickImgIndex.Clear;
  cmb_HoverImgIndex.Clear;
  cmb_SelectImgIndex.Clear;
  cmb_BlinkImgIndex.Clear;
  if Assigned(ImageList) then
  begin
    cmb_ClickImgIndex.ItemHeight := ImageList.Height + 2;
    cmb_HoverImgIndex.ItemHeight := ImageList.Height + 2;
    cmb_SelectImgIndex.ItemHeight := ImageList.Height + 2;
    cmb_BlinkImgIndex.ItemHeight := ImageList.Height + 2;

    for i := 0 to ImageList.Count - 1 do
    begin
      cmb_ClickImgIndex.Items.AddObject(inttostr(i), nil);
      cmb_HoverImgIndex.Items.AddObject(inttostr(i), nil);
      cmb_SelectImgIndex.Items.AddObject(inttostr(i), nil);
      cmb_BlinkImgIndex.Items.AddObject(inttostr(i), nil);
    end;
    //{$IFDEF DELPHI6_LVL}  // kh: please condider it again
    cmb_ClickImgIndex.Items.AddObject('(None)', nil);
    cmb_HoverImgIndex.Items.AddObject('(None)', nil);
    cmb_SelectImgIndex.Items.AddObject('(None)', nil);
    cmb_BlinkImgIndex.Items.AddObject('(None)', nil);
    //{$ENDIF}
        
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmHSIEditor.FormCreate(Sender: TObject);
begin
  FStatus := stNormal;
  HoverBorderItem := -1;
  FHoverItem := -1;
  FSelectedItem := -1;
  CurrentDelta := 0;
  PB.Width := FPicture.Width;
  PB.Height := FPicture.Height;
  LoadAllImageComboboxes;
  disablePolyTools;
  disableWandTools;
  lProperties(False);
  {$IFDEF DELPHI7_LVL}
  DoubleBuffered := true;
  pnImage.DoubleBuffered := true;
  pnButtons.DoubleBuffered := true;
  pnImage.DoubleBuffered := true;
  pnContainer.DoubleBuffered := true;
  pnProperties.DoubleBuffered := true;
  {$ENDIF}
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.PBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  i: Integer;

begin
  pnBackground.SetFocus;//so that the editboxes lose focus when mb is pressed
  case Button of
  mbLeft:
    begin
      //Create ellipse or rectangle
      if btRect.Down or btEllipse.Down then
      begin
        if FStatus = stDrawPoly then SavePoly;

          PStart.x := x;
          PStart.y := y;

        PCurrent := Point(X, Y);
        FStatus := stDrawing;
      end;

      if FStatus <> stDrawPoly then
        tmPoints := nil;

      if FStatus = stWand then
      begin
        PStart.x := X;
        PStart.y := Y;
        MagicWand(X,Y,trTol.Position/trTol.Max);
        PBPaint(Self);
      end;
      //Create polygon
      if btPoly.Down then
      begin
        PStart := Point(X, Y);
        PCurrent := Point(X, Y);
        if tmPoints <> nil then
        begin
          //if the user tries to close the polygon then finish drawing
          if (abs(tmPoints[0].x - tmPoints[High(tmPoints)].x) < CornerSize)and
             (abs(tmPoints[0].y - tmPoints[High(tmPoints)].y) < CornerSize) then
          begin
            tmPoints := Copy(tmPoints,0,High(tmPoints));
            if SavePoly then
              SelectedItem := FHotSpots.Count - 1;
            Exit;
          end;
          SetLength(tmPoints,Length(tmPoints) + 1);
          tmPoints[High(tmPoints)].x := X;
          tmPoints[High(tmPoints)].y := Y;
        end
        else
        begin
          FStatus := stDrawPoly;
          SetLength(tmPoints,2);
          Toolbar1.Enabled := False;
          tmPoints[0].x := X;
          tmPoints[0].y := Y;
          tmPoints[1].x := X;
          tmPoints[1].y := Y;
        end;
      end;

      //Editing mode
      if btNormal.Down then
      begin
        //If the mouse is on the border
        if HoverBorderItem <> -1 then
        begin
          if FHotSpots[HoverBorderItem].ShapeType = stPolygon then
          begin
            case FBorderPoly of
            bLine:
              begin
                PCurrent := Point(X, Y);
                FStatus := stMoveLine;
              end;
            bPoint:
              begin
                FStatus := stMovePoint;
              end;
            end;
            SelectedItem := HoverBorderItem;
            PBPaint(Self);
          end
          else
          begin
            if FHotSpots[HoverBorderItem].ShapeType = stEllipse then
            begin
              FStatus := stScaleEllipse;
              PStart := Point(X, Y);
              PCurrent := Point(X, Y);
              // WI:=FHotSpots[HoverBorderItem].Width;
              // HI:=FHotSpots[HoverBorderItem].Height;
              SelectedItem:=HoverBorderItem;
              PBPaint(Self);
            end;
          end;
        end
        else
          if FHoverItem <> -1 then
          begin
            // if SelectedItem=FHoverItem then//if it's already selected then move it
            // begin
            PCurrent.x := x;
            PCurrent.y := y;
            PStart.x := x;
            PStart.y := y;
            FStatus := stMove;
            svMove.x := FHotSpots[FHoverItem].x;
            svMove.y := FHotSpots[FHoverItem].y;
            SelectedItem := FHoverItem;
            // end else SelectedItem:= FHoverItem;//else select it
          end
          else
            SelectedItem := -1;
        end;

        //If Add point function is selected
        if (btAddPoint.Down) and (HoverBorderItem <> -1) and (FBorderPoly = bLine) then
        begin
          tmPoints := FHotSpots[HoverBorderItem].PolyPoints;
          SetLength(tmPoints,length(tmPoints) + 1);

          //if the point is between the last and the first
          if ((FPoint1=0)and(FPoint2=(high(tmPoints)-1))) or
             ((FPoint2=0)and(FPoint1=(high(tmPoints)-1))) then
          begin
            //Add the point at the end
            tmPoints[High(tmPoints)].x := x;
            tmPoints[High(tmPoints)].y := y;
            FHotSpots[HoverBorderItem].PolyPoints:=tmPoints;
          end
          else
          begin
            //else move all the points one position up
            for i := High(tmPoints) downto Max(FPoint1,FPoint2) + 1 do
            begin
              tmPoints[i] := tmPoints[i - 1];
            end;
            //add the point
            i := Max(FPoint1,FPoint2);
            tmPoints[i].x := x;
            tmPoints[i].y := y;
            //finally update the hotspot
            FHotSpots[HoverBorderItem].PolyPoints := tmPoints;
          end;
          //automatically select the normal button
          btNormal.Down := True;
          PBPaint(self);
          HotSpotsChanged:=true;
        end;

        //If Del line function is selected
        if (btDelLine.Down) and (HoverBorderItem <> -1) and (FBorderPoly = bLine) then
        begin
           //if it has less than 5 points delete the whole hotspot
          if Length(FHotSpots[HoverBorderItem].PolyPoints) <= 4 then
          begin
            btDeleteClick(Self);
            Exit;
          end;
          tmPoints := FHotSpots[HoverBorderItem].PolyPoints;

          //if the point is between the last and the first
          if ((FPoint1 = 0) and (FPoint2 = (High(tmPoints) - 1))) or
             ((FPoint2 = 0) and (FPoint1 = (High(tmPoints) - 1))) then
          begin
            //move the points down 1 position, but without the last point
            for i := Min(FPoint1,FPoint2) to High(tmPoints) - 2 do
              tmPoints[i] := tmPoints[i+1];
          end
          else
          begin
            //else move down the points two positions
            for i := Min(FPoint1,FPoint2) to High(tmPoints) - 2 do
              tmPoints[i] := tmPoints[i+2];
          end;
          //adjust the length of the array of points
          Setlength(tmPoints,length(tmPoints) - 2);
          //finally update the hotspot
          FHotSpots[HoverBorderItem].PolyPoints := tmPoints;
          // btNormal.Down := True; // disabled for wand generated hotspots which have too many points
          //                        // to make easyer editing
          PBPaint(Self);
          HotSpotsChanged:=true;
        end;
        // If Del point function is selected
        if (btDelPoint.Down) and (HoverBorderItem <> -1) and (FBorderPoly = bPoint) then
        begin
          if Length(FHotSpots[HoverBorderItem].PolyPoints) <= 3 then
          begin
            btDeleteClick(Self);
            Exit;
          end;
          tmPoints := FHotSpots[HoverBorderItem].PolyPoints;
          for i := FPoint1 to High(tmPoints) - 1 do
            tmPoints[i] := tmPoints[i + 1];
          Setlength(tmPoints,length(tmPoints) - 1);
          FHotSpots[HoverBorderItem].PolyPoints := tmPoints;
          // btNormal.Down := True; //disabled for wand generated hotspots which have too many points
          //                        //to make easyer editing
          PBPaint(self);
          HotSpotsChanged:=true;
        end;
      end;
  mbRight:
    begin
      if FStatus = stDrawPoly then
      begin
        if SavePoly then SelectedItem := FHotSpots.Count - 1;
        Exit;
      end;
      // To simulate mouse movement if the user repeatedly clicks the right mouse button
      PBMouseMove(Sender,Shift,X, Y);

      if (HoverBorderItem <> -1) or (FHoverItem <> -1) then
      begin
        if FHoverItem <> -1 then
          SelectedItem := FHoverItem
        else
          SelectedItem := HoverBorderItem;
        P := PB.ClientToScreen(Point(X, Y));
        pmHotSpot.Popup(P.X, P.Y);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  lbPozx.Caption := IntToStr(x);
  lbPozy.Caption := IntToStr(y);
  case FStatus of
  stNormal:
    begin
      PB.Cursor := crDefault;
      if btPoly.Down or btRect.Down or btEllipse.Down then
        Exit;
      for i := 0 to FHotSpots.Count - 1 do
      begin
        if FHotSpots[i].ShapeType = stPolygon then
        begin
          FBorderPoly := FHotSpots[i].getBorderPolyPos(x,y,FPoint1,FPoint2);
          case FBorderPoly of
          bNone:
            begin
              FHoverItem := -1;
              HoverBorderItem := -1;
              Continue;
            end;
          bPoint,bLine:
            begin
              HoverBorderItem := i;
              FHoverItem := -1;
              PB.Cursor := crHandPoint;
              Exit;
            end;
          bInside:
            begin
              FHoverItem := i;
              HoverBorderItem := -1;
              PB.Cursor := crHandPoint;
              Exit;
            end;
          end;
        end
        else
        begin
            FHoverPos := FHotSpots[i].GetHoverPos(X, Y);
            if not (FHoverPos in [hpNone, hpInside]) then
            begin
              HoverBorderItem := i;
              Break;
            end else HoverBorderItem := -1;
            if FHoverPos = hpInside then
            begin
              FHoverItem := i;
              Break;
            end
            else
              FHoverItem := -1;
          end;
        end;

        case FHoverPos of
        hpNone:PB.Cursor := crDefault;
        hpInside:PB.Cursor := crHandPoint;
        hpBorder:PB.Cursor := crSizeAll;
        end;
      end;
      //if drawing a rectangle or ellipse
  stDrawing:
    with PB.Canvas do
    begin
      X := RestrictX(X);
      Y := RestrictY(Y);

      Pen.Mode := pmNotXOR;
      Pen.Style := psDot;
      Pen.Width := 1;
      Pen.Color := clBlack;
      Brush.Style := bsClear;

      Rectangle(Round(PStart.X), Round(PStart.Y), Round(PCurrent.X), Round(PCurrent.Y));
      Rectangle(Round(PStart.X), Round(PStart.Y), X, Y);
      PCurrent.X := X;
      PCurrent.Y := Y;
    end;
  stDrawPoly:
    with PB.Canvas do
    begin
      X := RestrictX(X);
      Y := RestrictY(Y);

      Pen.Mode := pmNotXOR;
      Pen.Style := psDot;
      Pen.Width := 1;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      //draw the current line in XOR mode
      MoveTo(Round(PStart.X), Round(PStart.Y));
      LineTo(PCurrent.X, PCurrent.Y);

      MoveTo(PStart.X, PStart.Y);
      LineTo(X, Y);
      PCurrent.X := X;
      PCurrent.Y := Y;
      tmPoints[High(tmPoints)].x := x;
      tmPoints[High(tmPoints)].y := y;
    end;
  stMove:
    begin
      X := RestrictX(X);
      Y := RestrictY(Y);
      //making it snap to the original position
      if (Abs(PStart.x-x) >= 2) or (Abs(PStart.y-y) >= 2) then
      begin
        PCurrent := SubtractPoints(Point(x,y),PCurrent);
        FHotspots[SelectedItem].X1 := FHotspots[SelectedItem].X1+PCurrent.x;
        FHotspots[SelectedItem].Y1 := FHotspots[SelectedItem].Y1+PCurrent.y;
        PCurrent.x := x;
        PCurrent.y := y;
        end else
        begin
          FHotspots[SelectedItem].Y1 := svmove.y;
          FHotspots[SelectedItem].X1 := svmove.x;
        end;
        PBPaint(Self);
        HotSpotsChanged:=true;
      end;
  stMovePoint:
    begin
      X := RestrictX(X);
      Y := RestrictY(Y);
      FHotspots[HoverBorderItem].PolyPoint[FPoint1]:=(RPoint(x,y));
      PBPaint(Self);
      HotSpotsChanged:=true;
    end;
  stMoveLine:
    begin
      X := RestrictX(X);
      Y := RestrictY(Y);
      PCurrent := SubtractPoints(Point(x,y),PCurrent);
      FHotspots[HoverBorderItem].PolyPoint[FPoint1] := AddPoints(FHotspots[HoverBorderItem].PolyPoint[FPoint1],PCurrent);
      FHotspots[HoverBorderItem].PolyPoint[FPoint2] := AddPoints(FHotspots[HoverBorderItem].PolyPoint[FPoint2],PCurrent);
      PCurrent.x := x;
      PCurrent.y := y;
      PBPaint(Self);
      HotSpotsChanged:=true;
    end;
  stScaleEllipse:
    with PB.Canvas do
    begin
      FHotSpots[HoverBorderItem].scaleEllipse(Point(x,y),PCurrent,PStart);
      PCurrent := Point(x,y);
      PBPaint(Self);
      HotSpotsChanged:=true;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.PBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  tt: THotSpot;

begin
  case FStatus of
  //finish the hotspot(rect or ellipse)
  stDrawing:
    begin
      FStatus := stNormal;
      tt := FHotSpots.Add;
      tt.Clipped := True;

      tt.SetRect(PStart.X, PStart.Y, RestrictX(X), RestrictY(Y));
      if tt.Width < 10 then
        tt.Width := 10;
      if tt.Height < 10 then
        tt.Height := 10;
      if btRect.Down then
        tt.ShapeType := stPolygon;
      if btEllipse.Down then
        tt.ShapeType := stEllipse;
      btNormal.Down := True;
      SelectedItem := FHotSpots.Count - 1;
      PBPaint(Self);
      HotSpotsChanged := true;
    end;
  stMovePoint,stMoveLine:
    begin
      FStatus := stNormal;
      SelectedItem := HoverBorderItem;
      HoverBorderItem := -1;
      PBPaint(Self);
      HotSpotsChanged:=true;
    end;
  stMove:
    begin
      FStatus := stNormal;
      PBPaint(Self);
      HotSpotsChanged := true;
    end;
  stScaleEllipse:
    begin
      SelectedItem := HoverBorderItem;
      FStatus := stNormal;
      HoverBorderItem := -1;
      PBPaint(Self);
      HotSpotsChanged:=true;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TfrmHSIEditor.RestrictX(X: Integer): Integer;
begin
  Result := Max(Min(PB.Width, X), 0);
end;

//------------------------------------------------------------------------------
function TfrmHSIEditor.RestrictY(Y: Integer): Integer;
begin
  Result := Max(Min(PB.Height, Y), 0);
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.DrawHotSpotImage(HotSpot: THotSpot; Canvas: TCanvas);
var
  i, X, Y: Integer;
begin
  if not Assigned(HotSpot) or not Assigned(ImageList) then
    Exit;

  X := HotSpot.X + HotSpot.OffsetX;
  Y := HotSpot.Y + HotSpot.OffsetY;
  i := HotSpot.HoverImageIndex;
  if (i < 0) then
    i := HotSpot.ClickImageIndex;
  if (i < 0) then
    i := HotSpot.SelectedImageIndex;
  if (i < 0) then
    i := HotSpot.BlinkImageIndex;
  if (i >= 0) and (i < ImageList.Count) then
    ImageList.Draw(Canvas, X, Y, i);
end;

procedure TfrmHSIEditor.PBPaint(Sender: TObject);
var
  i,j: Integer;
  B: TBitmap;
  tp: array of TPoint;
  r:TRect;
begin
  B := TBitmap.Create;
  r.Left:=0;
  r.top:=0;
  r.Bottom:=FPicture.Height+CurrentDelta;
  r.Right:=FPicture.Width+CurrentDelta;
  with B.Canvas do
  try
    B.Width := PB.Width;
    B.Height := PB.Height;

    // fill the surface
    Brush.Style := bsSolid;
    Brush.Color := PB.Color;
    // Brush.Color := clBlack;

    FillRect(PB.BoundsRect);
    B.Canvas.StretchDraw(r, FPicture.Graphic);

    // draw the hotspots
    Brush.Style := bsClear;

    for i := 0 to FHotSpots.Count-1 do
    begin
      Pen.Mode := pmNotXOR;
      Pen.Style := psSolid;
      if i = HoverBorderItem then
      begin
        if FHotspots[i].ShapeType <> stPolygon then
        begin
          Pen.Color := clRed;
          Pen.Style := psSolid;
        end
        else
        begin
          Pen.Color := clBlack;
          if FStatus <> stNormal then
            Pen.Style := psDot
          else
            Pen.Style := psSolid;
        end;
      end
       else
        if i = FSelectedItem then
          Pen.Color := clBlue

        else
          Pen.Color := clBlack;

      // Modified to draw the right shape
      Brush.Style := bsClear;
      FHotspots[i].DrawShape(B.Canvas);
      DrawHotSpotImage(FHotspots[i], B.Canvas);

      //draw the rectangles in the points
      Pen.Mode := pmCopy;
      Pen.Color := clBlack;
      Brush.Style := bsSolid;
      Brush.Color := clBlack;
      if i = FSelectedItem then with FHotSpots[i] do
      begin
        if (PolyPoints <> nil) and ((FStatus = stNormal) or (FStatus = stMove)) then
        begin
          if ShapeType = stEllipse then
          begin
            for j := 0 to high(PolyPoints) do with PolyPoints[j] do
              if (j mod 3) = 0 then
                Rectangle(Round(x) - BordRectSz,Round(y)-BordRectSz,Round(x)+BordRectSz,Round(y)+BordRectSz);
          end
          else
          begin
            for j := 0 to high(PolyPoints) do
              with PolyPoints[j] do
                Rectangle(Round(x)-BordRectSz,Round(y)-BordRectSz,Round(x)+BordRectSz,Round(y)+BordRectSz);
          end;
        end;
      end;
    end;
    //draw the current magic wand selection


    if (FStatus = stWand) then
      lbNPoints.Caption := 'Poly Points: ' + IntToStr(length(tmPoints))
    else
      if (SelectedItem <> -1) then
          lbNPoints.Caption := 'Poly Points: '+IntToStr(length(FHotspots[SelectedItem].PolyPoints));

    if (FStatus = stWand) and (tmPoints <> nil) then
    begin
      Pen.Style := psDot;
      Pen.Mode := pmNotXor;
      Pen.Color := clBlack;
      Pen.Width := 1;

      Brush.Style := bsClear;

      SetLength(tp,Length(tmPoints));

      for i := 0 to High(tp) do
        tp[i] := PRound(tmPoints[i]);

      PolyLine(tp);
    end;

  finally
    PB.Canvas.Draw(0, 0, B);
    B.Free;
    // during the polydraw, draw the polygon with the xor pen
    if (FStatus = stDrawPoly) and (tmPoints <> nil) then
    with PB.Canvas do
    begin
      Pen.Style := psDot;
      Pen.Mode := pmNotXor;
      Pen.Color := clBlack;
      Pen.Width := 1;
      Brush.Style := bsClear;

      SetLength(tp,Length(tmPoints)-1);
      for i := 0 to High(tp) do
        tp[i] := PRound(tmPoints[i]);
      PolyLine(tp);

      MoveTo(PStart.X, PStart.Y);
      LineTo(PCurrent.X, PCurrent.Y);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.miDeleteClick(Sender: TObject);
var
  temp:integer;
begin
  temp := SelectedItem;
  SelectedItem := -1;
  FHotSpots.Delete(temp);
  HoverBorderItem := -1;
  FHoverItem:= -1;
  FHoverPos := hpNone;
  PB.Cursor := crDefault;
  PBPaint(Self);
  HotSpotsChanged:=true;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btLoadClickClick(Sender: TObject);
begin
  OPD.Title := 'Clicked picture';
  if FSelectedItem=-1 then
    Exit;
  if OPD.Execute then
    ImgClick.Picture.LoadFromFile(OPD.FileName);
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btClearClickClick(Sender: TObject);
begin
  if (FSelectedItem = -1) then
    Exit;
  ImgClick.Picture.Assign(nil);
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btLoadHoverClick(Sender: TObject);
begin
  OPD.Title := 'Hover picture';
  if FSelectedItem = -1 then
    Exit;
  if OPD.Execute then
    ImgHover.Picture.LoadFromFile(OPD.FileName);
end;

//------------------------------------------------------------------------------

procedure TfrmHSIEditor.btClearHoverClick(Sender: TObject);
begin
  if FSelectedItem = -1 then
    Exit;
  ImgHover.Picture.Assign(nil);
end;

//------------------------------------------------------------------------------

function TfrmHSIEditor.GetImageIndex(cmb: TComboBox): Integer;
begin
  Result := -1;
  if Assigned(ImageList) then
  begin
    Result := cmb.ItemIndex;
    if (Result = ImageList.Count) then
      Result := -1;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmHSIEditor.SetImageIndex(cmb: TComboBox; ImageIndex: Integer);
begin
  if Assigned(ImageList) and Assigned(cmb) then
  begin
    if (ImageIndex < 0) then
      cmb.ItemIndex := ImageList.Count
    else
      cmb.ItemIndex := ImageIndex;
  end;
end;

//------------------------------------------------------------------------------

function GetPanelColor(Panel: TPanel): TColor;
begin
  Result := clNone;
  if Assigned(Panel) then
    if (Panel.Caption = '') then
      Result := Panel.Color;
end;

procedure SetPanelColor(Panel: TPanel; Clr: TColor);
begin
  if Assigned(Panel) then
  begin
    if (Clr = clNone) then
    begin
      Panel.Caption := 'None';
      Panel.Color := clBtnFace;
    end
    else
    begin
      Panel.Color := Clr;
      Panel.Caption := '';
    end;
  end;
end;

function GetShapeColor(Panel: TShape): TColor;
begin
  Result := clNone;
  if Assigned(Panel) then
//    if (Panel.Caption = '') then
    if (Panel.Tag = 1) then
      Result := Panel.Brush.Color;
end;

procedure SetShapeColor(Panel: TShape; Clr: TColor);
begin
  if Assigned(Panel) then
  begin
    if (Clr = clNone) then
    begin
      Panel.Tag := 0; // 'None';
      Panel.Brush.Color := clWhite;
    end
    else
    begin
      Panel.Brush.Color := Clr;
      Panel.Tag := 1;
      //Panel.Caption := '';
    end;
  end;
end;



procedure TfrmHSIEditor.SetSelectedItem(const Value: Integer);
begin
  if FSelectedItem <> Value then
  begin
    // save the properties for the old selected hotspot
    if FSelectedItem <> -1 then
    with FHotSpots[FSelectedItem] do
    begin
      Hint := txtHint.Text;
      HoverImage.Assign(imgHover.Picture);
      ClickImage.Assign(imgClick.Picture);
      SelectedImage.Assign(Img_Selected.Picture);
      BlinkImage.Assign(Img_Blink.Picture);
      HoverColor := GetShapeColor(Shape_HoverColor); // Pn_HoverColor.Color;
      ClickColor := GetShapeColor(Shape_ClickColor); //Pn_ClickColor.Color;
      SelectedColor := GetShapeColor(Shape_SelectedColor); //Pn_SelectedColor.Color;
      BlinkColor := GetShapeColor(Shape_BlinkColor); //Pn_BlinkColor.Color;
      HoverImageIndex := GetImageIndex(cmb_HoverImgIndex);
      ClickImageIndex := GetImageIndex(cmb_ClickImgIndex);
      SelectedImageIndex := GetImageIndex(cmb_SelectImgIndex);
      BlinkImageIndex := GetImageIndex(cmb_BlinkImgIndex);
      OffsetX := se_OffsetX.Value;
      OffsetY := se_OffsetY.Value;
      HotSpotsChanged:=true;
      // Added the name and id
      try
        Id := StrToInt(edtId.Text);
      except
        on EConvertError do
          ID := 0;
      end;
      Name := edtName.Text;
      if ShapeType = stPolygon then
        SavePoly;
    end;

    // load the images (and the name&ID) of the newly selected hotspot
    FSelectedItem := Value;
    PBPaint(Self);
    lProperties(FSelectedItem <> -1);
    if pnProperties.Enabled then
      pnTitle.Color := clActiveCaption
    else
      pnTitle.Color := clInactiveCaption;

    if FSelectedItem <> -1 then
    with FHotSpots[FSelectedItem] do
    begin
      if ShapeType = stPolygon then
        EnablePolyTools
      else
        DisablePolyTools;

      txtHint.Text := Hint;
      ImgHover.Picture.Assign(HoverImage);
      ImgClick.Picture.Assign(ClickImage);
      Img_Selected.Picture.Assign(SelectedImage);
      Img_Blink.Picture.Assign(BlinkImage);
      Pn_HoverColor.Color := HoverColor;
      SetShapeColor(Shape_HoverColor, HoverColor);
      Pn_ClickColor.Color := ClickColor;
      SetShapeColor(Shape_ClickColor, ClickColor);
      Pn_SelectedColor.Color := SelectedColor;
      SetShapeColor(Shape_SelectedColor, SelectedColor);
      Pn_BlinkColor.Color := BlinkColor;
      SetShapeColor(Shape_BlinkColor, BlinkColor);
      SetImageIndex(cmb_HoverImgIndex, HoverImageIndex);
      SetImageIndex(cmb_ClickImgIndex, ClickImageIndex);
      SetImageIndex(cmb_SelectImgIndex, SelectedImageIndex);
      SetImageIndex(cmb_BlinkImgIndex, BlinkImageIndex);

      se_OffsetX.Value := OffsetX;
      se_OffsetY.Value := OffsetY;

      edtName.Text := Name;
      edtID.Text := IntToStr(Id);
      spAngle.Value := Angle;
      ckClip.Checked := Clipped;
      chk_Blink.Checked := Blink;
      ckSelectable.Checked := Selectable;
      ToolBar1.Enabled:=true;
      btCopyToClipboard.Enabled := True;
      btDelete.Enabled := True;
    end
    else
    begin
      DisablePolyTools;
      txtHint.Text := '';
      edtId.Text := '';
      edtName.Text := '';
      spAngle.Value := 0;
      imgHover.Picture.Bitmap.Width := 0;
      imgHover.Picture.Bitmap.Height := 0;
      imgClick.Picture.Bitmap.Width := 0;
      imgClick.Picture.Bitmap.Height := 0;
      img_Selected.Picture.Bitmap.Width := 0;
      img_Selected.Picture.Bitmap.Height := 0;
      img_Blink.Picture.Bitmap.Width := 0;
      img_Blink.Picture.Bitmap.Height := 0;
      SetImageIndex(cmb_HoverImgIndex, -1);
      SetImageIndex(cmb_ClickImgIndex, -1);
      SetImageIndex(cmb_SelectImgIndex, -1);
      SetImageIndex(cmb_BlinkImgIndex, -1);

      se_OffsetX.Value := 4;
      se_OffsetY.Value := 4;

      btCopyToClipboard.Enabled := False;
      btDelete.Enabled := False;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btRectClick(Sender: TObject);
begin
  if FStatus = stWand then
  begin
    DisableWandTools;
    SavePoly;
    tmPoints := nil;
  end;
  if FStatus = stDrawPoly then
  begin
    SavePoly;
  end;
  FStatus := stNormal;
  SelectedItem := -1;
  PBPaint(Self);
end;


//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btOkClick(Sender: TObject);
begin
  btZoomRSTClick(Sender);
 // simulate the unselection of the item in order to save the hint
  if FStatus = stWand then
    SavePoly;
  SelectedItem := -1;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.miClearClickImageClick(Sender: TObject);
begin
  with imgClick.Picture.Bitmap do
  begin
    Width := 0;
    Height := 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.miClearHoverImageClick(Sender: TObject);
begin
  with imgHover.Picture.Bitmap do
  begin
    Width := 0;
    Height := 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.miSaveHSImageClick(Sender: TObject);
var
  Source, Dest: TBitmap;
begin
  if not Assigned(FPicture) then
    Exit;

  if SPD.Execute then
  begin
    Source := TBitmap.Create;
    Dest := TBitmap.Create;
    with FHotSpots[FSelectedItem] do
    try
      Source.Width := FPicture.Width;
      Source.Height := FPicture.Height;
      Source.Canvas.Draw(0,0,FPicture.Graphic);

      Dest.Width := X2-X1;
      Dest.Height := Y2-Y1;
      Dest.Canvas.CopyRect(
        Rect(0,0,Dest.Width,Dest.Height),
        Source.Canvas,
        AsRect);
    finally
      Dest.SaveToFile(SPD.FileName);
      Dest.Free;
      Source.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.FormResize(Sender: TObject);
//var
  //tt: Integer;
begin
  {tt := txtHint.Top-10-pnClick.Top;
  pnHover.Height := (tt) div 2;
  pnClick.Height := pnHover.Height;
  pnHover.Top := pnClick.Top+pnClick.Height;}
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btBackImageClick(Sender: TObject);
var
 sft:TShiftState;
begin
  OPD.Title := 'Background picture';
  if OPD.Execute then
  begin
    sft:=[];
    PBMouseDown(PB,mbLeft,sft,-1,-1);
    PBMouseUP(PB,mbLeft,sft,-1,-1);
    FPicture.LoadFromFile(OPD.FileName);
    //Dragos
    CurrentDelta:=0;  // Remouve this if you don't Reset Zoom factour
    PB.Width:=FPicture.Width + CurrentDelta;
    PB.Height:=FPicture.Height + CurrentDelta;
    if FHotSpots_org.Count>0 then
    begin
       if MessageDlg('Clear HotSpots ?', mtConfirmation, [mbYes, mbNo], 0)= mrYes  then
    Begin
      FHotSpots_org.Clear;
      Fhotspots.Clear;
    End;
    end
      else
      begin
        FHotSpots_org.Clear;
        Fhotspots.Clear;
      end;
    PBPaint(Self);
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btEllipseClick(Sender: TObject);
begin
  if FStatus = stWand then
  begin
    DisableWandTools;
    SavePoly;
    tmPoints := nil;
  end;
  SelectedItem := -1;
  if FStatus = stDrawPoly then
  begin
    savePoly;
  end;
  FStatus := stNormal;
  PBPaint(Self);
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  temp: Integer;
begin
  if (Key = Ord('I')) and (ssCtrl in Shift) and not (ssShift in Shift) then
  begin
    btZoomInClick(Self);
    Exit;
  end;
  if (Key = Ord('I')) and (ssCtrl in Shift) and (ssShift in Shift) then
  begin
    btZoomOutClick(Self);
    Exit;
  end;
  if (Key = ord('C')) and (ssctrl in Shift) then
  begin
    btCopyToClipBoardClick(Self);
    Exit;
  end;


  if (FStatus = stDrawPoly) and ((Key = vk_Return) or (Key = vk_Escape)) then
  begin
    if SavePoly then
      SelectedItem := FHotSpots.Count - 1;
  end;



  if (SelectedItem = -1) or (Key <> vk_Delete) then
    Exit;

  if edtId.Focused or edtName.Focused or txtHint.Focused or spAngle.Focused or se_OffsetX.Focused or se_OffsetY.Focused then
    Exit;

  temp := SelectedItem;
  SelectedItem := -1;
  FHotSpots.Delete(temp);
  HoverBorderItem := -1;
  FHoverItem := -1;
  FHoverPos := hpNone;
  PB.Cursor := crDefault;
  PBPaint(Self);
  HotSpotsChanged:=true;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btDeleteClick(Sender: TObject);
var
  temp: Integer;
begin
  if (SelectedItem = -1) then
    Exit;
  temp := SelectedItem;
  SelectedItem := -1;
  FHotSpots.Delete(temp);
  HoverBorderItem := -1;
  FHoverItem := -1;
  FHoverPos := hpNone;
  PB.Cursor := crDefault;
  btNormal.Down := True;
  PBPaint(Self);
  HotSpotsChanged:=true;
end;

procedure TfrmHSIEditor.btDelLineClick(Sender: TObject);
begin
  
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.edtIDKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Key > '9') or (Key < '0')) and (Key > ' ') then
    Key := #0;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.edtNameMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//  edtName.SelectAll;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.edtIDMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//  edtID.SelectAll;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btPolyClick(Sender: TObject);
begin
  if FStatus = stWand then
  begin
    DisableWandTools;
    SavePoly;
    FStatus := stNormal;
    tmPoints := nil;
  end;
  SelectedItem := -1;
  PBPaint(Self);
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.spAngleChange(Sender: TObject);
begin
  if SelectedItem = -1 then
    Exit;
  FHotSpots[SelectedItem].Angle := spAngle.Value;
  PBPaint(Self);
  HotSpotsChanged:=true;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btCopyToClipBoardClick(Sender: TObject);
var
  MyFormat : Word;
  Bitmap,Source : TBitMap;
  AData: THandle;
  APalette : HPalette;
  copyRect:Trect;
begin
  if SelectedItem < 0 then
    Exit;
  Bitmap := TBitmap.Create;
  Source := TBitmap.Create;
  try
    Source.Width := PB.width;
    Source.Height :=PB.Height;
    CopyRect := Rect(0,0,Source.Width,Source.Height);
    Source.Canvas.StretchDraw(copyRect, FPicture.Graphic);
    with FHotSpots[SelectedItem] do
    begin
      Bitmap.Width := Width;
      Bitmap.Height := Height;
      Bitmap.Canvas.CopyRect(Rect(0,0,Bitmap.Width,Bitmap.Height),Source.Canvas, AsRect);
    end;
    Bitmap.SaveToClipBoardFormat(MyFormat,AData,APalette);
    ClipBoard.SetAsHandle(MyFormat,AData);
  finally
    Bitmap.Free;
    Source.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btNormalClick(Sender: TObject);
begin
  if FStatus = stWand then
  begin
    DisableWandTools;
    if SavePoly then
      SelectedItem := FHotSpots.Count - 1;
    tmPoints := nil;
  end;
  if FStatus = stDrawPoly then
  begin
    if SavePoly then
      SelectedItem := FHotSpots.Count - 1;
  end;
  FStatus := stNormal;
  PBPaint(Self);
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.ckClipClick(Sender: TObject);
begin
  if SelectedItem <> -1 then
    FHotSpots[SelectedItem].Clipped := ckClip.Checked;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btWandClick(Sender: TObject);
begin
  PB.Cursor := crDefault;
  PStart.x := -1;
  PStart.y := -1;
  if FStatus = stDrawPoly then
    SavePoly;
  SelectedItem := -1;
  tmPoints := nil;
  FStatus := stWand;
  EnableWandTools;
  PBPaint(Self);
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.trTolChange(Sender: TObject);
begin
  if (PStart.x<0) or (PStart.y<0) then
    Exit;
  MagicWand(PStart.x,PStart.y,trTol.Position/trTol.Max);
  PBPaint(Self);
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.LoadSettings(inifile: TFileName);
var
  ini:TIniFile;
begin
  ini := TIniFile.Create(inifile);
  try
    Top := ini.ReadInteger('Size','Top',Top);
    Left := ini.ReadInteger('Size','Left',Left);
    Height := ini.ReadInteger('Size','Height',Height);
    Width := ini.ReadInteger('Size','Width',Width);
    pnBackground.Width := ini.ReadInteger('Splitter','Position',pnBackground.Width);
    trDensity.Position := ini.ReadInteger('Wand','Accuracy',trDensity.Position);
    trTol.Position := ini.ReadInteger('Wand','Tolerance',trTol.Position);
    seZoomRatio.Value:= ini.ReadInteger('Zoom','Ratio',seZoomRatio.Value);
  finally
    ini.free;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.SaveSettings(inifile: TFileName);
var
  ini:TIniFile;
begin
  ini:=TIniFile.Create(inifile);
  try
    ini.WriteInteger('Size','Top',Top);
    ini.WriteInteger('Size','Left',Left);
    ini.WriteInteger('Size','Height',Height);
    ini.WriteInteger('Size','Width',Width);
    ini.WriteInteger('Splitter','Position',pnBackground.Width);
    ini.WriteInteger('Wand','Accuracy',trDensity.Position);
    ini.WriteInteger('Wand','Tolerance',trTol.Position);
    ini.WriteInteger('Zoom','Ratio',seZoomRatio.Value);
  finally
    ini.free;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmHSIEditor.btZoomInClick(Sender: TObject);
begin
  if HotSpotsChanged then SwapHotSpots;
  FHotSpots.Assign(FHotSpots_org);
  CurrentDelta := CurrentDelta + seZoomRatio.Value;
  FHotSpots.ReScale(CurrentDelta);
  PB.Width := FPicture.Width + CurrentDelta;
  PB.Height := FPicture.Height + CurrentDelta;
  PBPaint(Self);
end;

procedure TfrmHSIEditor.btZoomOutClick(Sender: TObject);
var
  i:integer;
begin
  i := CurrentDelta - seZoomRatio.Value;
  if FPicture.Width+i <= 0 then
    Exit;
  if FPicture.Height+i <= 0 then
    Exit;
  if HotSpotsChanged then
    SwapHotSpots;
  FHotSpots.Assign(FHotSpots_org);
  CurrentDelta := i;
  FHotSpots.ReScale(CurrentDelta);
  PB.Width := FPicture.Width + CurrentDelta;
  PB.Height := FPicture.Height + CurrentDelta;
  PBPaint(Self);
end;

procedure TfrmHSIEditor.SpeedButton1Click(Sender: TObject);
begin
  fhotspots.Clear;
  HotSpotsChanged := true;
  PBPaint(Self);
end;

procedure TfrmHSIEditor.SwapHotSpots;
begin
  FHotSpots_org.Assign(FHotSpots);
  FHotSpots_org.ReScale(FPicture.Width,FPicture.Height);
  HotSpotsChanged := false;
end;

procedure TfrmHSIEditor.btZoomRSTClick(Sender: TObject);
begin
  if HotSpotsChanged then
    SwapHotSpots;
  FHotSpots.Assign(FHotSpots_org);
  CurrentDelta := 0;
  PB.Width := FPicture.Width;
  PB.Height := FPicture.Height;
end;

function TfrmHSIEditor.GetSelectedItem: integer;
begin
  if FSelectedItem >= FHotSpots.Count then
    FSelectedItem := -1;
  Result := FSelectedItem;  
end;

function TfrmHSIEditor.getHoverBorderItem: integer;
begin
  if FHoverBorderItem>=FHotSpots.Count then
      FHoverBorderItem:=-1;
  result:=FHoverBorderItem;
end;

procedure TfrmHSIEditor.SetHoverBorderItem(const Value: integer);
begin
  if value<FHotSpots.Count then FHoverBorderItem:=value
                           else FHoverBorderItem:=-1;
end;

procedure TfrmHSIEditor.Pn_BlinkColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    Pn_BlinkColor.Color := ColorDialog1.Color;
    SetShapeColor(Shape_BlinkColor, ColorDialog1.Color);
  end;
end;

procedure TfrmHSIEditor.Pn_ClickColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    Pn_ClickColor.Color := ColorDialog1.Color;
    SetShapeColor(Shape_ClickColor, ColorDialog1.Color);
    //SetPanelColor(Shape_ClickColor, ColorDialog1.Color);
  end;
end;

procedure TfrmHSIEditor.Pn_HoverColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    Pn_HoverColor.Color := ColorDialog1.Color;
    SetShapeColor(Shape_HoverColor, ColorDialog1.Color);
  end;
end;

procedure TfrmHSIEditor.Pn_SelectedColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    Pn_SelectedColor.Color := ColorDialog1.Color;
    SetShapeColor(Shape_SelectedColor, ColorDialog1.Color);
  end;
end;

procedure TfrmHSIEditor.Btn_SelectedClick(Sender: TObject);
begin
  OPD.Title := 'Selected picture';
  if FSelectedItem =-1 then
    Exit;
  if OPD.Execute then
    Img_Selected.Picture.LoadFromFile(OPD.FileName);
end;

procedure TfrmHSIEditor.Btn_BlinkClick(Sender: TObject);
begin
  OPD.Title := 'Blink picture';
  if FSelectedItem=-1 then
    Exit;
  if OPD.Execute then
    Img_Blink.Picture.LoadFromFile(OPD.FileName);
end;

procedure TfrmHSIEditor.chk_BlinkClick(Sender: TObject);
begin
  if SelectedItem <> -1 then
    FHotSpots[SelectedItem].Blink := chk_Blink.Checked;
end;

procedure TfrmHSIEditor.mi_CLearSelectImageClick(Sender: TObject);
begin
  with img_Selected.Picture.Bitmap do
  begin
    Width := 0;
    Height := 0;
  end;
end;

procedure TfrmHSIEditor.mi_ClearBlinkImageClick(Sender: TObject);
begin
  with img_Blink.Picture.Bitmap do
  begin
    Width := 0;
    Height := 0;
  end;
end;

procedure TfrmHSIEditor.se_OffsetXChange(Sender: TObject);
begin
  if SelectedItem = -1 then
    Exit;
  FHotSpots[SelectedItem].OffsetX := se_OffsetX.Value;
  PBPaint(Self);
  HotSpotsChanged:=true;
end;

procedure TfrmHSIEditor.se_OffsetYChange(Sender: TObject);
begin
  if SelectedItem = -1 then
    Exit;
  FHotSpots[SelectedItem].OffsetY := se_OffsetY.Value;
  PBPaint(Self);
  HotSpotsChanged:=true;
end;


procedure TfrmHSIEditor.cmb_ClickImgIndexChange(Sender: TObject);
begin
  if SelectedItem = -1 then
    Exit;
  FHotSpots[SelectedItem].ClickImageIndex := GetImageIndex(cmb_ClickImgIndex);
  PBPaint(Self);
  HotSpotsChanged:=true;
end;

procedure TfrmHSIEditor.cmb_HoverImgIndexChange(Sender: TObject);
begin
  if SelectedItem = -1 then
    Exit;
  FHotSpots[SelectedItem].HoverImageIndex := GetImageIndex(cmb_HoverImgIndex);
  PBPaint(Self);
  HotSpotsChanged:=true;
end;

procedure TfrmHSIEditor.cmb_SelectImgIndexChange(Sender: TObject);
begin
  if SelectedItem = -1 then
    Exit;
  FHotSpots[SelectedItem].SelectedImageIndex := GetImageIndex(cmb_SelectImgIndex);
  PBPaint(Self);
  HotSpotsChanged:=true;
end;

procedure TfrmHSIEditor.cmb_BlinkImgIndexChange(Sender: TObject);
begin
  if SelectedItem = -1 then
    Exit;
  FHotSpots[SelectedItem].BlinkImageIndex := GetImageIndex(cmb_BlinkImgIndex);
  PBPaint(Self);
  HotSpotsChanged:=true;
end;

procedure TfrmHSIEditor.cmb_BlinkImgIndexDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  t: Integer;
begin
  if not Assigned(Control) or not Assigned(ImageList) or not(Control is TComboBox) then
    Exit;

  TComboBox(Control).Canvas.FillRect(Rect);
  t := TComboBox(Control).Canvas.TextHeight('Wg');
  t := (Rect.Bottom - Rect.Top - t) div 2;
  if (Index < ImageList.Count) then
  begin
    ImageList.Draw(TComboBox(Control).Canvas, Rect.Left + 2, Rect.Top, Index);
    TComboBox(Control).Canvas.TextOut(Rect.Left + 8 + ImageList.Width, Rect.Top + t, InttoStr(Index));
  end
  else
    TComboBox(Control).Canvas.TextOut(Rect.Left + 4, Rect.Top + t, '(None)');
end;

procedure TfrmHSIEditor.btClearSelectedClick(Sender: TObject);
begin
  if FSelectedItem =-1 then
    Exit;
  Img_Selected.Picture.Assign(nil);
end;

procedure TfrmHSIEditor.btClearBlinkClick(Sender: TObject);
begin
  if FSelectedItem=-1 then
    Exit;
  Img_Blink.Picture.Assign(nil);
end;

procedure TfrmHSIEditor.bt_ClearClickColorClick(Sender: TObject);
begin
  SetShapeColor(Shape_ClickColor, clNone);
end;

procedure TfrmHSIEditor.bt_ClearHoverColorClick(Sender: TObject);
begin
  SetShapeColor(Shape_HoverColor, clNone);
end;

procedure TfrmHSIEditor.bt_ClearSelectColorClick(Sender: TObject);
begin
  SetShapeColor(Shape_SelectedColor, clNone);
end;

procedure TfrmHSIEditor.bt_ClearBlinkColorClick(Sender: TObject);
begin
  SetShapeColor(Shape_BlinkColor, clNone);
end;

procedure TfrmHSIEditor.ckSelectableClick(Sender: TObject);
begin
  if SelectedItem <> -1 then
    FHotSpots[SelectedItem].Selectable := ckSelectable.Checked;
end;

initialization
  FHotSpots := THotSpots.Create(nil);
  FHotSpots_org := THotSpots.Create(nil);
  FPicture := TPicture.Create;

finalization
  FHotSpots.Free;
  FHotSpots_org.Free;
  FPicture.Free;

end.
