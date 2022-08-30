{===============================================================================
  RzToolbarForm Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzToolbarForm
    Form file used by TRzLookupDialog component.


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzToolbarForm;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Buttons,
  StdCtrls,
  RzPanel,
  RzLabel,
  ExtCtrls,
  Menus,
  RzSpnEdt,
  RzBorder,
  RzRadGrp,
  Mask,
  RzEdit,
  RzButton,
  RzLstBox,
  RzRadChk,
  ExtDlgs, 
  RzCmboBx, 
  RzChkLst;


type
  TRzFrmCustomizeToolbar = class(TForm)
    BtnClose: TRzButton;
    LstControls: TRzCheckList;
    Timer1: TTimer;
    BtnSpacer: TSpeedButton;
    BtnGrooveSpacer: TSpeedButton;
    LblHint: TRzLabel;
    LblTextOptions: TRzLabel;
    CbxTextOptions: TRzComboBox;
    BtnMoveUp: TRzButton;
    BtnMoveDown: TRzButton;
    procedure BtnMoveDownClick(Sender: TObject);
    procedure BtnMoveUpClick(Sender: TObject);
    procedure LstControlsClick(Sender: TObject);
    procedure LstControlsDragDrop( Sender, Source: TObject; X, Y: Integer);
    procedure LstControlsDragOver( Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean );
    procedure ListBoxDrawItem( Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState );
    procedure LstControlsEndDrag( Sender, Target: TObject; X, Y: Integer );
    procedure LstControlsMouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure Timer1Timer(Sender: TObject);
    procedure CbxTextOptionsClick(Sender: TObject);
    procedure LstControlsChange(Sender: TObject; Index: Integer;
      NewState: TCheckBoxState);
    procedure FormCreate(Sender: TObject);
  private
    OldIdx, NewIdx: Integer;
    GoingUp: Boolean;

    procedure EnableMoveButtons( Idx: Integer );
  public
    SelectedBtn: TRzToolbarButton;
    CompOwner: TComponent;
    Toolbar: TRzToolbar;
    procedure UpdateControls( CustomizeCaptions: TRzCustomizeCaptions; ShowTextOptions: Boolean );
    procedure Reposition;
  end;


implementation

{$R *.dfm}

uses
  RzCommon,
  IniFiles,
  Registry,
  ImgList;

{====================================}
{== TRzFrmCustomizeToolbar Methods ==}
{====================================}

procedure TRzFrmCustomizeToolbar.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;
  Position := poDesigned;
end;


procedure TRzFrmCustomizeToolbar.Reposition;
var
  T, L: Integer;
  R, DeskRect: TRect;
  P: TPoint;
begin
  if ( Toolbar <> nil ) and ( CompOwner <> nil ) then
  begin
    R := Toolbar.BoundsRect;
    case Toolbar.Align of
      alNone:
        P := Point( R.Left, R.Bottom );
      alLeft:
        P := Point( R.Right, R.Top );
      alTop:
        P := Point( R.Left, R.Bottom );
      alRight:
        P := Point( R.Left - Width - 5, R.Top );
      alBottom:
        P := Point( R.Left, R.Top - Height - 5 );
    end;

    if CompOwner is TControl then
      P := TControl( CompOwner ).ClientToScreen( P );

    T := P.Y + 25;
    L := P.X + 15;
    DeskRect := GetDesktopClientRect;
    if L < 0 then
      L := 0;
    if L + Width > DeskRect.Right then
      L := DeskRect.Right - Width;

    if T < 0 then
      T := 0;
    if T + Height > DeskRect.Bottom then
      T := DeskRect.Bottom - Height;

    Top := T;
    Left := L;
  end;
end;


procedure TRzFrmCustomizeToolbar.UpdateControls( CustomizeCaptions: TRzCustomizeCaptions; ShowTextOptions: Boolean );
var
  I: Integer;
  C: TControl;
  S: string;
begin
  Caption := CustomizeCaptions.Title;
  LblHint.Caption := CustomizeCaptions.Hint;
  BtnClose.Caption := CustomizeCaptions.Close;
  BtnMoveUp.Caption := CustomizeCaptions.MoveUp;
  BtnMoveDown.Caption := CustomizeCaptions.MoveDown;
  LblTextOptions.Caption := CustomizeCaptions.TextOptions;

  CbxTextOptions.Items.Add( CustomizeCaptions.NoTextLabels );
  CbxTextOptions.Items.Add( CustomizeCaptions.ShowTextLabels );
  CbxTextOptions.Items.Add( CustomizeCaptions.SelectiveTextOnRight );

  if Toolbar.TextOptions <> ttoCustom then
    CbxTextOptions.ItemIndex := Ord( Toolbar.TextOptions );

  if not ShowTextOptions then
  begin
    CbxTextOptions.Visible := False;
    LblTextOptions.Visible := False;
    ClientHeight := LstControls.Top + LstControls.Height + 8;
  end;

  for I := 0 to Toolbar.ToolbarControls.Count - 1 do
  begin
    C := TRzToolbarControl( Toolbar.ToolbarControls[ I ] ).Control;
    S := '';

    if C is TRzToolButton then
      S := TRzToolButton( C ).Caption;

    if S = '' then
    begin
      if C is TRzSpacer then
        S := 'Spacer'
      else if C.Hint <> '' then
        S := C.Hint
      else
        S := C.Name;
    end;

    LstControls.Items.Add( S );
    LstControls.ItemChecked[ I ] := TRzToolbarControl( Toolbar.ToolbarControls[ I ] ).Control.Visible;
  end;


  BtnMoveUp.Enabled := LstControls.Items.Count > 0;
  BtnMoveDown.Enabled := LstControls.Items.Count > 0;
  if LstControls.Items.Count > 0 then
  begin
    LstControls.ItemIndex := 0;
    EnableMoveButtons( 0 );
  end;
end;


procedure TRzFrmCustomizeToolbar.EnableMoveButtons( Idx: Integer );
begin
  BtnMoveUp.Enabled := Idx > 0;
  BtnMoveDown.Enabled := Idx < LstControls.Items.Count - 1;
end;


procedure TRzFrmCustomizeToolbar.BtnMoveUpClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := LstControls.ItemIndex;
  if Idx <> -1 then
  begin
    LstControls.Items.BeginUpdate;
    try
      LstControls.Items.Move( Idx, Idx - 1 );
      Toolbar.ToolbarControls.Move( Idx, Idx - 1 );
      Toolbar.PositionControls;
      LstControls.ItemIndex := Idx - 1;
    finally
      LstControls.Invalidate;
      LstControls.Items.EndUpdate;
    end;
    EnableMoveButtons( Idx - 1 );
  end;
end;

procedure TRzFrmCustomizeToolbar.BtnMoveDownClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := LstControls.ItemIndex;
  if Idx <> -1 then
  begin
    LstControls.Items.BeginUpdate;
    try
      LstControls.Items.Move( Idx, Idx + 1 );
      Toolbar.ToolbarControls.Move( Idx, Idx + 1 );
      Toolbar.PositionControls;
      LstControls.ItemIndex := Idx + 1;
    finally
      LstControls.Invalidate;
      LstControls.Items.EndUpdate;
    end;
    EnableMoveButtons( Idx + 1 );
  end;
end;

procedure TRzFrmCustomizeToolbar.LstControlsClick( Sender: TObject );
begin
  EnableMoveButtons( LstControls.ItemIndex );
end;


procedure TRzFrmCustomizeToolbar.LstControlsDragDrop( Sender, Source: TObject; X, Y: Integer );
begin
  if NewIdx <> -1 then
  begin
    LstControls.Items.BeginUpdate;
    try
      LstControls.Items.Move( OldIdx, NewIdx );
      Toolbar.ToolbarControls.Move( OldIdx, NewIdx );
      Toolbar.PositionControls;
      LstControls.ItemIndex := NewIdx;
      EnableMoveButtons( LstControls.ItemIndex );
    finally
      LstControls.Invalidate;
      LstControls.Items.EndUpdate;
    end;
  end;
end;


procedure TRzFrmCustomizeToolbar.LstControlsDragOver( Sender, Source: TObject; X, Y: Integer; State: TDragState;
                                                      var Accept: Boolean );
begin
  NewIdx := ( Sender as TRzCheckList ).ItemAtPos( Point( X, Y ), True );
  Accept := ( Source = Sender ) and ( NewIdx <> -1 );
  if Accept then
  begin
    with Sender as TRzCheckList do
    begin
      if Y > Height - ItemHeight then
      begin
        GoingUp := False;
        Timer1.Enabled := True
      end
      else if Y < ItemHeight then
      begin
        GoingUp := True;
        Timer1.Enabled := True
      end
      else
        Timer1.Enabled := False;
      ItemIndex := NewIdx;
    end;
  end;
end;


procedure TRzFrmCustomizeToolbar.ListBoxDrawItem( Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState );
var
  Bmp: TBitmap;
  TextOffset: Integer;
  R, DestRct, SrcRct: TRect;
  TransColor: TColor;
  C: TControl;
  SB: TSpeedButton;
  TB: TRzToolButton;
  SP: TRzSpacer;

  function GetTransparentColor( B: TBitmap ): TColor;
  begin
    Result := B.Canvas.Pixels[ 0, B.Height - 1 ];
  end;

begin
  C := TRzToolbarControl( Toolbar.ToolbarControls[ Index ] ).Control;

  LstControls.Canvas.FillRect( R );                   { Clear area for icon and text }

  if C is TRzToolButton then
  begin
    TB := C as TRzToolButton;
    if TB.ImageList <> nil then
    begin
      TB.ImageList.Draw( LstControls.Canvas, Rect.Left + 2, Rect.Top + 2, TB.ImageIndex );
    end;
  end
  else if C is TSpeedButton then
  begin
    SB := C as TSpeedButton;

    Bmp := TBitmap.Create;
    try
      DestRct := Classes.Rect( 0, 0, SB.Glyph.Width div SB.NumGlyphs, SB.Glyph.Height );
      SrcRct := DestRct;

      { Don't forget to set the Width and Height of destination bitmap. }
      Bmp.Width := SB.Glyph.Width div SB.NumGlyphs;
      Bmp.Height := SB.Glyph.Height;

      TransColor := GetTransparentColor( SB.Glyph );
      Bmp.Canvas.BrushCopy( DestRct, SB.Glyph, SrcRct, TransColor);

      LstControls.Canvas.Draw( Rect.Left + 2, Rect.Top + 2, Bmp );
    finally
      Bmp.Free;
    end;
  end
  else if C is TRzSpacer then
  begin
    SP := C as TRzSpacer;

    Bmp := TBitmap.Create;
    try
      DestRct := Classes.Rect( 0, 0, 16, 16 );
      SrcRct := DestRct;

      { Don't forget to set the Width and Height of destination bitmap. }
      Bmp.Width := 16;
      Bmp.Height := 16;

      TransColor := GetTransparentColor( BtnSpacer.Glyph );
      if SP.Grooved then
        Bmp.Canvas.BrushCopy( DestRct, BtnGrooveSpacer.Glyph, SrcRct, TransColor )
      else
        Bmp.Canvas.BrushCopy( DestRct, BtnSpacer.Glyph, SrcRct, TransColor );

      LstControls.Canvas.Draw( Rect.Left + 2, Rect.Top + 2, Bmp );
    finally
      Bmp.Free;
    end;
  end;


  R := Rect;
  Inc( R.Left, 28 );
  TextOffset := ( LstControls.ItemHeight - LstControls.Canvas.TextHeight( 'Pp' ) ) div 2;
  LstControls.Canvas.TextRect( R, R.Left + 2, R.Top + TextOffset, LstControls.Items[ Index ] );
end;


procedure TRzFrmCustomizeToolbar.LstControlsEndDrag( Sender, Target: TObject; X, Y: Integer );
begin
  Timer1.Enabled := False;
end;


procedure TRzFrmCustomizeToolbar.LstControlsMouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState;
                                                       X, Y: Integer );
begin
  OldIdx := LstControls.ItemIndex;
end;


procedure TRzFrmCustomizeToolbar.Timer1Timer( Sender: TObject );
begin
  if GoingUp then
    if LstControls.TopIndex > 0 then
      LstControls.TopIndex := LstControls.TopIndex - 1
    else
      Timer1.Enabled := False
  else
    if LstControls.TopIndex < LstControls.Items.Count - 1 then
      LstControls.TopIndex := LstControls.TopIndex + 1
    else
      Timer1.Enabled := False;
end;


procedure TRzFrmCustomizeToolbar.CbxTextOptionsClick( Sender: TObject );
begin
  if Toolbar <> nil then
  begin
    case CbxTextOptions.ItemIndex of
      0: Toolbar.TextOptions := ttoNoTextLabels;
      1: Toolbar.TextOptions := ttoShowTextLabels;
      2: Toolbar.TextOptions := ttoSelectiveTextOnRight;
    end;
  end;
end;

procedure TRzFrmCustomizeToolbar.LstControlsChange( Sender: TObject; Index: Integer; NewState: TCheckBoxState );
begin
  Toolbar.ToolbarControls[ Index ].Control.Visible := NewState = cbChecked;
  Toolbar.PositionControls;
end;

end.




