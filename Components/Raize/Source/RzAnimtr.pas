{===============================================================================
  RzAnimtr Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzAnimator
    Cycles through images in a ImageList


  Modification History
  ------------------------------------------------------------------------------
  6.1.4  (29 May 2013)
    * Fixed issue where background of TRzAnimator would not be painted correctly
      if the parent's DoubleBuffered property was True.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Added new Continous property to TRzAnimator. This property is True by
      default, which describes the traditional functionality of this control.
      However, setting continuous to False, causes the control to animate
      through its set of images one time and then stop. At the end of the
      animation, the Animate property is set to False.  Reset Animate to True
      to replay the animation, or simply call the new StartAnimation method.
    * Added OnCycleComplete event to TRzAnimator, which fires when the last
      frame of the animation cycle is displayed.
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in
      TRzAnimator.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added DelayUntilRepeat property to TRzAnimator. This property controls the
      amount of time (in milliseconds) that pass before the animation repeats.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Added OnFrameChange event to TRzAnimator.  This event fires when a new
      image is drawn during the animation.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Cleaned-up image list code.
    * Changed ImageList to type TCustomImageList.
    * Added TChangeLink reference.
    * Added Transparent property.
===============================================================================}

{$I RzComps.inc}

unit RzAnimtr;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  {&RF}
  Messages,
  Windows,
  Controls,
  Graphics,
  SysUtils,
  ExtCtrls,
  ImgList,
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Classes,
  RzCommon;

type
  TRzAnimatorFrameChangeEvent = procedure( Sender: TObject; Frame: Integer ) of object;

  TRzAnimator = class( TCustomControl )
  private
    FAboutInfo: TRzAboutInfo;
    FAnimate: Boolean;
    FContinuous: Boolean;
    FDelay: Word;
    FDelayUntilRepeat: Word;
    FLastImageIndex: TImageIndex;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FBitmap: TBitmap;
    FTimer: TTimer;
    FTransparent: Boolean;

    FOnFrameChange: TRzAnimatorFrameChangeEvent;
    FOnCycleComplete: TNotifyEvent;

    { Internal Event Handlers }
    procedure TimerExpired( Sender: TObject );
    procedure ImagesChange( Sender: TObject );

    { Message Handling Methods }
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure DrawImage; virtual;
    procedure Paint; override;

    procedure FrameChange( Frame: Integer ); dynamic;
    procedure CycleComplete; dynamic;

    { Property Access Methods }
    procedure SetAnimate( Value: Boolean ); virtual;
    procedure SetContinuous( Value: Boolean ); virtual;
    procedure SetDelay( Value: Word ); virtual;
    procedure SetImageIndex( Value: TImageIndex ); virtual;
    procedure SetImageList( Value: TCustomImageList ); virtual;
    procedure SetTransparent( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure StartAnimation;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Animate: Boolean
      read FAnimate
      write SetAnimate
      default True;

    property Continuous: Boolean
      read FContinuous
      write SetContinuous
      default True;

    property Delay: Word
      read FDelay
      write SetDelay
      default 100;

    property DelayUntilRepeat: Word
      read FDelayUntilRepeat
      write FDelayUntilRepeat
      default 100;

    property ImageIndex: TImageIndex
      read FImageIndex
      write SetImageIndex
      default 0;

    property ImageList: TCustomImageList
      read FImages
      write SetImageList;

    property Transparent: Boolean
      read FTransparent
      write SetTransparent
      default False;

    property OnFrameChange: TRzAnimatorFrameChangeEvent
      read FOnFrameChange
      write FOnFrameChange;

    property OnCycleComplete: TNotifyEvent
      read FOnCycleComplete
      write FOnCycleComplete;

    { Inherited Properties & Events }
    property Color;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}

    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
  end;


implementation


uses
  {&RAS}
  RzGrafx;

{&RT}
{=========================}
{== TRzAnimator Methods ==}
{=========================}

constructor TRzAnimator.Create( AOwner: TComponent );
begin
  inherited;
  Height := 40;
  Width := 40;
  FAnimate := True;
  FContinuous := True;
  FLastImageIndex := -1;
  FImageIndex := 0;
  FDelay := 100;                                            { 100 milliseconds }
  FDelayUntilRepeat := 100;

  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChange;

  FBitmap := TBitmap.Create;
  FTimer := TTimer.Create( Self );
  FTimer.Interval := FDelay;
  FTimer.Enabled := True;
  FTimer.OnTimer := TimerExpired;
  {&RCI}
end;


destructor TRzAnimator.Destroy;
begin
  FBitmap.Free;
  FTimer.Free;
  FImagesChangeLink.Free;
  inherited;
end;


procedure TRzAnimator.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FImages ) then
    SetImageList( nil )  // Call access method so connections to link object can be cleared
end;


procedure TRzAnimator.DrawImage;
var
  R: TRect;
begin
  FBitmap.Height := FImages.Height;
  FBitmap.Width := FImages.Width;

  if FTransparent then
  begin
    if ( Parent <> nil ) and Parent.DoubleBuffered then
      PerformEraseBackground( Self, FBitmap.Canvas.Handle );
    DrawParentImage( Self, FBitmap.Canvas );
    Sleep( 10 );  { Need to allow short time to get image }
  end
  else
  begin
    FBitmap.Canvas.Brush.Color := Color;
    R := Rect( 0, 0, FBitmap.Width + 1, FBitmap.Height + 1 );
    FBitmap.Canvas.FillRect( R );
  end;

  FImages.GetBitmap( FImageIndex, FBitmap );
  Canvas.Draw( 0, 0, FBitmap );

  if FLastImageIndex <> FImageIndex then
    FrameChange( FImageIndex );
  FLastImageIndex := FImageIndex;
end;


procedure TRzAnimator.FrameChange( Frame: Integer );
begin
  if Assigned( FOnFrameChange ) then
    FOnFrameChange( Self, Frame );
end;


procedure TRzAnimator.CycleComplete;
begin
  if Assigned( FOnCycleComplete ) then
    FOnCycleComplete( Self );
end;


procedure TRzAnimator.Paint;
begin
  inherited;

  if csDesigning in ComponentState then
  begin
    if not FAnimate and ( FImages <> nil ) and ( FImages.Count > 0 ) then
      FImages.Draw( Canvas, 0, 0, FImageIndex );

    Canvas.Pen.Style := psDot;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle( ClientRect );
  end
  else if FImages <> nil then
  begin
    DrawImage;
  end;
end; {= TRzAnimator.Paint =}


procedure TRzAnimator.TimerExpired( Sender: TObject );
begin
  if FImages <> nil then
  begin
    try
      FTimer.Interval := FDelay;
      if FImageIndex >= FImages.Count then
        FImageIndex := 0;

      DrawImage;

      Inc( FImageIndex );
      if FImageIndex = FImages.Count then
      begin
        FImageIndex := 0;
        if FContinuous then
          FTimer.Interval := FDelayUntilRepeat
        else
        begin
          Animate := False;
          CycleComplete;
        end;
      end;
    except
      Animate := False;
      raise;
    end;
  end;
end;


procedure TRzAnimator.StartAnimation;
begin
  Animate := True;
end;


procedure TRzAnimator.SetAnimate( Value: Boolean );
begin
  FAnimate := Value;
  FTimer.Enabled := FAnimate;

  if FImages <> nil then
  begin
    if FAnimate then
      FImageIndex := 0
    else
    begin
      FImageIndex := FImages.Count - 1;
      DrawImage;
    end;
  end;
  {&RV}
end;


procedure TRzAnimator.SetContinuous( Value: Boolean );
begin
  if FContinuous <> Value then
  begin
    FContinuous := Value;
    Animate := FContinuous;
  end;
end;


procedure TRzAnimator.SetDelay( Value: Word );
begin
  if FDelay <> Value then
  begin
    FDelay := Value;
    FTimer.Interval := FDelay;
  end;
end;


procedure TRzAnimator.SetImageIndex( Value: TImageIndex );
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;


procedure TRzAnimator.SetImageList( Value: TCustomImageList );
begin
  if FImages <> nil then
    FImages.UnRegisterChanges( FImagesChangeLink );

  FImages := Value;

  if FImages <> nil then
  begin
    FImages.RegisterChanges( FImagesChangeLink );
    FImages.FreeNotification( Self );
    Width := FImages.Width;
    Height := FImages.Height;
  end;
  Invalidate;
end;


procedure TRzAnimator.ImagesChange( Sender: TObject );
begin
  if Sender = ImageList then
    Update;         // Call Update instead of Invalidate to prevent flicker
end;


procedure TRzAnimator.SetTransparent( Value: Boolean );
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;


procedure TRzAnimator.WMSize( var Msg: TWMSize );
begin
  inherited;

  if FImages <> nil then
  begin
    Width := FImages.Width;
    Height := FImages.Height;
  end;
end;



{&RUIF}
end.
