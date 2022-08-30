{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressNavBar                                            }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING    }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxNavBarStyles;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Graphics, dxCoreClasses, cxClasses, dxNavBarBase, cxGeometry;

type
  TdxBarStyleHAlignment = (haLeft, haCenter, haRight);
  TdxBarStyleVAlignment = (vaTop, vaCenter, vaBottom);
  TdxBarStyleAssignedValue = (savAlphaBlending, savAlphaBlending2, savBackColor,
    savBackColor2, savFont, savGradientMode, savHAlignment, savVAlignment, savImage);
  TdxBarStyleAssignedValues = set of TdxBarStyleAssignedValue;
  TdxBarStyleGradientMode = (gmHorizontal, gmVertical, gmForwardDiagonal, gmBackwardDiagonal);

  { TdxNavBarBaseStyle }

  TdxNavBarBaseStyle = class(TcxOwnedPersistent)
  private
    FAlphaBlending: Byte;
    FAlphaBlending2: Byte;
    FAssignedValues: TdxBarStyleAssignedValues;
    FBackColor: TColor;
    FBackColor2: TColor;
    FFont: TFont;
    FGradientMode: TdxBarStyleGradientMode;
    FHAlignment: TdxBarStyleHAlignment;
    FImage: TPicture;
    FScaleFactor: TdxScaleFactor;
    FVAlignment: TdxBarStyleVAlignment;

    FOnChange: TdxNavBarChangeEvent;

    function IsAlphaBlending2Stored: Boolean;
    function IsAlphaBlendingStored: Boolean;
    function IsGradientModeStored: Boolean;
    function IsHAlignmentStored: Boolean;
    function IsVAlignmentStored: Boolean;
    procedure SetAlphaBlending(const Value: Byte);
    procedure SetAlphaBlending2(const Value: Byte);
    procedure SetAssignedValues(const Value: TdxBarStyleAssignedValues);
    procedure SetBackColor(const Value: TColor);
    procedure SetBackColor2(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetGradientMode(const Value: TdxBarStyleGradientMode);
    procedure SetHAlignment(const Value: TdxBarStyleHAlignment);
    procedure SetImage(const Value: TPicture);
    procedure SetVAlignment(const Value: TdxBarStyleVAlignment);
  protected
    function CanAssignValue: Boolean; virtual;
    function CanScaleFont: Boolean; virtual;
    procedure Changed(AType: TdxNavBarChangeType); virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure FontChanged(Sender: TObject);
    procedure ImageChanged(Sender: TObject);

    property ScaleFactor: TdxScaleFactor read FScaleFactor;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure ResetValues;

    property AssignedValues: TdxBarStyleAssignedValues read FAssignedValues write SetAssignedValues default [];
    property OnChange: TdxNavBarChangeEvent read FOnChange write FOnChange;
  published
    property AlphaBlending: Byte read FAlphaBlending write SetAlphaBlending stored IsAlphaBlendingStored;
    property AlphaBlending2: Byte read FAlphaBlending2 write SetAlphaBlending2 stored IsAlphaBlending2Stored;
    property BackColor: TColor read FBackColor write SetBackColor;
    property BackColor2: TColor read FBackColor2 write SetBackColor2;
    property GradientMode: TdxBarStyleGradientMode  read FGradientMode write SetGradientMode stored IsGradientModeStored;
    property Font: TFont read FFont write SetFont;
    property Image: TPicture read FImage write SetImage;
    property HAlignment: TdxBarStyleHAlignment read FHAlignment write SetHAlignment stored IsHAlignmentStored;
    property VAlignment: TdxBarStyleVAlignment read FVAlignment write SetVAlignment stored IsVAlignmentStored;
  end;

  { TdxNavBarDefaultStyle }

  TdxNavBarDefaultStyleAssignProc = procedure of object;
  TdxNavBarDefaultStyles = class;
  TdxNavBarDefaultStyle = class(TdxNavBarBaseStyle)
  strict private
    FDefaultValues: Boolean;

    function GetCollection: TdxNavBarDefaultStyles;
  protected
    FDefaultValuesProc: TdxNavBarDefaultStyleAssignProc;

    procedure AssignDefaultValues(CheckDefault: Boolean);
    function CanScaleFont: Boolean; override;
    procedure Changed(AType: TdxNavBarChangeType); override;
    procedure ChangeScale(M, D: Integer); override;
  public
    constructor Create(AOwner: TPersistent); override;
    //
    property Collection: TdxNavBarDefaultStyles read GetCollection;
    property DefaultValues: Boolean read FDefaultValues write FDefaultValues;
  published
    property AssignedValues;
  end;

  { TdxNavBarDefaultStyles }

  TdxNavBarDefaultStyles = class(TcxOwnedPersistent)
  strict private
    FDefaultStylesList: TList;
    FUpdateCount: Integer;

    FOnChange: TdxNavBarChangeEvent;

    function IsDefaultStyleStored(Index: Integer): Boolean;
    function GetDefaultStyleCount: Integer;
    function GetDefaultStyle(Index: Integer): TdxNavBarDefaultStyle;
    function GetIsDesigning: Boolean;
    procedure SetDefaultStyle(Index: Integer; const Value: TdxNavBarDefaultStyle);

    procedure AssignDefaultBackgroundStyle;
    procedure AssignDefaultButtonStyle;
    procedure AssignDefaultButtonPressedStyle;
    procedure AssignDefaultButtonHotTrackedStyle;
    procedure AssignDefaultGroupBackgroundStyle;
    procedure AssignDefaultGroupControlStyle;
    procedure AssignDefaultGroupHeaderStyle;
    procedure AssignDefaultGroupHeaderActiveStyle;
    procedure AssignDefaultGroupHeaderActiveHotTrackedStyle;
    procedure AssignDefaultGroupHeaderActivePressedStyle;
    procedure AssignDefaultGroupHeaderHotTrackedStyle;
    procedure AssignDefaultGroupHeaderPressedStyle;
    procedure AssignDefaultHintStyle;
    procedure AssignDefaultItemStyle;
    procedure AssignDefaultItemDisabledStyle;
    procedure AssignDefaultItemHotTrackedStyle;
    procedure AssignDefaultItemPressedStyle;
    procedure AssignDefaultDropTargetGroupStyle;
    procedure AssignDefaultDropTargetLinkStyle;
    procedure AssignDefaultNavigationPaneHeaderStyle;
    procedure AssignDefaultChildGroupCaptionStyle;
    procedure AssignDefaultChildGroupCaptionHotTrackedStyle;
    procedure AssignDefaultChildGroupCaptionPressedStyle;
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    procedure CreateDefaultStyleList; virtual;
    procedure DestroyDefaultStyleList; virtual;
    procedure OnStyleChange(Sender: TObject; AType: TdxNavBarChangeType);
    //
    property IsDesigning: Boolean read GetIsDesigning;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignDefaultValues(CheckDefault: Boolean);
    procedure BeginUpdate;
    procedure EndUpdate;

    property DefaultStyleCount: Integer read GetDefaultStyleCount;
    property DefaultStyles[Index: Integer]: TdxNavBarDefaultStyle read GetDefaultStyle;
    property OnChange: TdxNavBarChangeEvent read FOnChange write FOnChange;
  published
    property Background: TdxNavBarDefaultStyle index 0 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property Button: TdxNavBarDefaultStyle index 1 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property ButtonPressed: TdxNavBarDefaultStyle index 2 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property ButtonHotTracked: TdxNavBarDefaultStyle index 3 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property GroupBackground: TdxNavBarDefaultStyle index 4 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property GroupControl: TdxNavBarDefaultStyle index 5 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property GroupHeader: TdxNavBarDefaultStyle index 6 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property GroupHeaderActive: TdxNavBarDefaultStyle index 7 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property GroupHeaderActiveHotTracked: TdxNavBarDefaultStyle index 8 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property GroupHeaderActivePressed: TdxNavBarDefaultStyle index 9 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property GroupHeaderHotTracked: TdxNavBarDefaultStyle index 10 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property GroupHeaderPressed: TdxNavBarDefaultStyle index 11 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property Hint: TdxNavBarDefaultStyle index 12 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property Item: TdxNavBarDefaultStyle index 13 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property ItemDisabled: TdxNavBarDefaultStyle index 14 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property ItemHotTracked: TdxNavBarDefaultStyle index 15 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property ItemPressed: TdxNavBarDefaultStyle index 16 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property DropTargetGroup: TdxNavBarDefaultStyle index 17 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property DropTargetLink: TdxNavBarDefaultStyle index 18 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property NavigationPaneHeader: TdxNavBarDefaultStyle index 19 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property ChildGroupCaption: TdxNavBarDefaultStyle index 20 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property ChildGroupCaptionHotTracked: TdxNavBarDefaultStyle index 21 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
    property ChildGroupCaptionPressed: TdxNavBarDefaultStyle index 22 read GetDefaultStyle write SetDefaultStyle stored IsDefaultStyleStored;
  end;

  { TdxNavBarStyle }

  TdxNavBarStyle = class(TdxNavBarBaseStyle)
  published
    property AssignedValues;
  end;

  { TdxNavBarStyleItem }

  TdxNavBarStyleItem = class(TdxNavBarComponentCollectionItem)
  strict private
    FChangeStyleHandlers: TcxEventHandlerCollection;
    FStyle: TdxNavBarStyle;

    procedure SetStyle(const Value: TdxNavBarStyle);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    procedure OnStyleChange(Sender: TObject; AType: TdxNavBarChangeType);
    procedure RegisterChanges(AValue: TcxEventHandler);
    procedure UnRegisterChanges(AValue: TcxEventHandler);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Style: TdxNavBarStyle read FStyle write SetStyle;
  end;

  { TdxNavBarStyleRepository }

  TdxNavBarStyleRepository = class(TdxNavBarComponentCollection)
  strict private
    function GetItem(Index: Integer): TdxNavBarStyleItem;
    procedure SetItem(Index: Integer; Value: TdxNavBarStyleItem);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); override;
  public
    function Add: TdxNavBarStyleItem;
    property Items[Index: Integer]: TdxNavBarStyleItem read GetItem write SetItem; default;
  end;

  { TdxNavBarBaseCustomStyles }

  TdxNavBarBaseCustomStyles = class(TdxNavBarNexusPersistent)
  strict private
    FStyles: array of TdxNavBarStyleItem;

    FOnChange: TdxNavBarChangeEvent;

    procedure InternalStyleChange(Sender: TObject; const AEventArgs);
  protected
    procedure AddNotificator(AIndex: Integer); virtual;
    procedure DoChange;
    function GetStyle(Index: Integer): TdxNavBarStyleItem;
    procedure SetStyle(Index: Integer; const Value: TdxNavBarStyleItem);
    function GetStyleCount: Integer; virtual; abstract;
    procedure FreeNotification(AComponent: TComponent); override;
    procedure RemoveNotificator(AIndex: Integer); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Styles[Index: Integer]: TdxNavBarStyleItem read GetStyle write SetStyle;
    property OnChange: TdxNavBarChangeEvent read FOnChange write FOnChange;
  end;

  { TdxNavBarCustomStyles }

  TdxNavBarCustomStyles = class(TdxNavBarBaseCustomStyles)
  protected
    function GetStyleCount: Integer; override;
  published
    property Background: TdxNavBarStyleItem index 0 read GetStyle write SetStyle;
    property Hint: TdxNavBarStyleItem index 1 read GetStyle write SetStyle;
    property DropTargetGroup: TdxNavBarStyleItem index 2 read GetStyle write SetStyle;
    property DropTargetLink: TdxNavBarStyleItem index 3 read GetStyle write SetStyle;
    property Button: TdxNavBarStyleItem index 4 read GetStyle write SetStyle;
    property ButtonHotTracked: TdxNavBarStyleItem index 5 read GetStyle write SetStyle;
    property ButtonPressed: TdxNavBarStyleItem index 6 read GetStyle write SetStyle;
    property NavigationPaneHeader: TdxNavBarStyleItem index 7 read GetStyle write SetStyle;
  end;

  { TdxNavBarGroupCustomStyles }

  TdxNavBarGroupCustomStyles = class(TdxNavBarBaseCustomStyles)
  protected
    function GetStyleCount: Integer; override;
  published
    property Background: TdxNavBarStyleItem index 0 read GetStyle write SetStyle;
    property Control: TdxNavBarStyleItem index 1 read GetStyle write SetStyle;
    property Header: TdxNavBarStyleItem index 2 read GetStyle write SetStyle;
    property HeaderActive: TdxNavBarStyleItem index 3 read GetStyle write SetStyle;
    property HeaderActiveHotTracked: TdxNavBarStyleItem index 4 read GetStyle write SetStyle;
    property HeaderActivePressed: TdxNavBarStyleItem index 5 read GetStyle write SetStyle;
    property HeaderHotTracked: TdxNavBarStyleItem index 6 read GetStyle write SetStyle;
    property HeaderPressed: TdxNavBarStyleItem index 7 read GetStyle write SetStyle;
    property ChildCaption: TdxNavBarStyleItem index 8 read GetStyle write SetStyle;
    property ChildCaptionHotTracked: TdxNavBarStyleItem index 9 read GetStyle write SetStyle;
    property ChildCaptionPressed: TdxNavBarStyleItem index 10 read GetStyle write SetStyle;
  end;

  { TdxNavBarItemCustomStyles }

  TdxNavBarItemCustomStyles = class(TdxNavBarBaseCustomStyles)
  protected
    function GetStyleCount: Integer; override;
  published
    property Item: TdxNavBarStyleItem index 0 read GetStyle write SetStyle;
    property ItemDisabled: TdxNavBarStyleItem index 1 read GetStyle write SetStyle;
    property ItemHotTracked: TdxNavBarStyleItem index 2 read GetStyle write SetStyle;
    property ItemPressed: TdxNavBarStyleItem index 3 read GetStyle write SetStyle;
  end;

  { TdxNavBarStyleOptions }

  TdxNavBarStyleOptions = class(TcxOwnedPersistent)
  strict private
    FDefaultStyles: TdxNavBarDefaultStyles;
    FCustomStyles: TdxNavBarCustomStyles;
    FCustomStyleRepository: TdxNavBarStyleRepository;

    FOnChange: TNotifyEvent;

    procedure CustomStylesChanged(Sender: TObject; AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification);
    procedure StyleChanged(Sender: TObject; AType: TdxNavBarChangeType);
    procedure SetCustomStyleRepository(Value: TdxNavBarStyleRepository);
    procedure SetDefaultStyles(Value: TdxNavBarDefaultStyles);
    procedure SetCustomStyles(Value: TdxNavBarCustomStyles);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property DefaultStyles: TdxNavBarDefaultStyles read FDefaultStyles write SetDefaultStyles;
    property CustomStyles: TdxNavBarCustomStyles read FCustomStyles write SetCustomStyles;
    property CustomStyleRepository: TdxNavBarStyleRepository read FCustomStyleRepository write SetCustomStyleRepository;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  dxNavBar, SysUtils, cxGraphics, dxDPIAwareUtils;

type
  TdxCustomNavBarAccess = class(TdxCustomNavBar);

{ TdxNavBarStyle }

constructor TdxNavBarBaseStyle.Create(AOwner: TPersistent);
begin
  inherited;
  FBackColor := clWhite;
  FBackColor2 := clWhite;
  FAlphaBlending := 255;
  FAlphaBlending2 := 255;
  FVAlignment := vaCenter;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FImage := TPicture.Create;
  FImage.OnChange := ImageChanged;
  FScaleFactor := TdxScaleFactor.Create;
end;

destructor TdxNavBarBaseStyle.Destroy;
begin
  FreeAndNil(FScaleFactor);
  FreeAndNil(FImage);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxNavBarBaseStyle.Assign(Source: TPersistent);
begin
  if Source is TdxNavBarBaseStyle then
  begin
    FAlphaBlending := TdxNavBarBaseStyle(Source).AlphaBlending;
    FAlphaBlending2 := TdxNavBarBaseStyle(Source).AlphaBlending2;
    FBackColor := TdxNavBarBaseStyle(Source).BackColor;
    FBackColor2 := TdxNavBarBaseStyle(Source).BackColor2;
    FGradientMode := TdxNavBarBaseStyle(Source).GradientMode;
    dxAssignFont(FFont, TdxNavBarBaseStyle(Source).Font, ScaleFactor, TdxNavBarBaseStyle(Source).ScaleFactor);
    FImage.Assign(TdxNavBarBaseStyle(Source).Image);
    FHAlignment := TdxNavBarBaseStyle(Source).HAlignment;
    FVAlignment := TdxNavBarBaseStyle(Source).VAlignment;
    FAssignedValues := TdxNavBarBaseStyle(Source).AssignedValues;
    Changed(doRecalc);
  end
  else
    inherited Assign(Source);
end;

procedure TdxNavBarBaseStyle.ResetValues;
begin
  FAlphaBlending := 255;
  FAlphaBlending2 := 255;
  FBackColor := clWhite;
  FBackColor2 := clWhite;
  FGradientMode := gmHorizontal;

  cxResetFont(FFont);
  FFont.Height := ScaleFactor.Apply(FFont.Height, dxSystemScaleFactor);
  FImage.Graphic := nil;
  FHAlignment := haLeft;
  FVAlignment := vaCenter;
  FAssignedValues := [];
end;

function TdxNavBarBaseStyle.CanAssignValue: Boolean;
begin
  Result := True;
end;

function TdxNavBarBaseStyle.CanScaleFont: Boolean;
begin
  Result := savFont in AssignedValues;
end;

procedure TdxNavBarBaseStyle.Changed(AType: TdxNavBarChangeType);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, AType);
end;

procedure TdxNavBarBaseStyle.ChangeScale(M, D: Integer);
begin
  ScaleFactor.Change(M, D);
  if CanScaleFont then
    Font.Height := MulDiv(Font.Height, M, D);
end;

procedure TdxNavBarBaseStyle.FontChanged(Sender: TObject);
begin
  if CanAssignValue then
    Include(FAssignedValues, savFont);
  Changed(doRecalc);
end;

procedure TdxNavBarBaseStyle.ImageChanged(Sender: TObject);
begin
  if CanAssignValue then
    Include(FAssignedValues, savImage);
  Changed(doRecalc);
end;

function TdxNavBarBaseStyle.IsAlphaBlendingStored: Boolean;
begin
  Result := savAlphaBlending in FAssignedValues;
end;

function TdxNavBarBaseStyle.IsAlphaBlending2Stored: Boolean;
begin
  Result := savAlphaBlending2 in FAssignedValues;
end;

function TdxNavBarBaseStyle.IsGradientModeStored: Boolean;
begin
  Result := savGradientMode in FAssignedValues;
end;

function TdxNavBarBaseStyle.IsHAlignmentStored: Boolean;
begin
  Result := savHAlignment in FAssignedValues;
end;

function TdxNavBarBaseStyle.IsVAlignmentStored: Boolean;
begin
  Result := savVAlignment in FAssignedValues;
end;

procedure TdxNavBarBaseStyle.SetAlphaBlending(const Value: Byte);
begin
  if FAlphaBlending <> Value then
  begin
    FAlphaBlending := Value;
    if CanAssignValue then
      Include(FAssignedValues, savAlphaBlending);
    Changed(doRedraw);
  end;
end;

procedure TdxNavBarBaseStyle.SetAlphaBlending2(const Value: Byte);
begin
  if FAlphaBlending2 <> Value then
  begin
    FAlphaBlending2 := Value;
    if CanAssignValue then
      Include(FAssignedValues, savAlphaBlending2);
    Changed(doRedraw);
  end;
end;

procedure TdxNavBarBaseStyle.SetAssignedValues(const Value: TdxBarStyleAssignedValues);
begin
  if FAssignedValues <> Value then
  begin
    FAssignedValues := Value;
    Changed(doRecalc);
  end;
end;

procedure TdxNavBarBaseStyle.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    if CanAssignValue then
      Include(FAssignedValues, savBackColor);
    Changed(doRedraw);
  end;
end;

procedure TdxNavBarBaseStyle.SetBackColor2(const Value: TColor);
begin
  if FBackColor2 <> Value then
  begin
    FBackColor2 := Value;
    if CanAssignValue then
      Include(FAssignedValues, savBackColor2);
    Changed(doRedraw);
  end;
end;

procedure TdxNavBarBaseStyle.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  if CanAssignValue then
    Include(FAssignedValues, savFont);
  Changed(doRecalc);
end;

procedure TdxNavBarBaseStyle.SetGradientMode(const Value: TdxBarStyleGradientMode);
begin
  if FGradientMode <> Value then
  begin
    FGradientMode := Value;
    if CanAssignValue then
      Include(FAssignedValues, savGradientMode);
    Changed(doRedraw);
  end;
end;

procedure TdxNavBarBaseStyle.SetHAlignment(const Value: TdxBarStyleHAlignment);
begin
  if FHAlignment <> Value then
  begin
    FHAlignment := Value;
    if CanAssignValue then
      Include(FAssignedValues, savHAlignment);
    Changed(doRedraw);
  end;
end;

procedure TdxNavBarBaseStyle.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
  Changed(doRecalc);
end;

procedure TdxNavBarBaseStyle.SetVAlignment(const Value: TdxBarStyleVAlignment);
begin
  if FVAlignment <> Value then
  begin
    FVAlignment := Value;
    if CanAssignValue then
      Include(FAssignedValues, savVAlignment);
    Changed(doRedraw);
  end;
end;

{ TdxNavBarDefaultStyle }

constructor TdxNavBarDefaultStyle.Create(AOwner: TPersistent);
begin
  inherited;
  FDefaultValues := True;
end;

function TdxNavBarDefaultStyle.CanScaleFont: Boolean;
begin
  Result := inherited CanScaleFont or (Collection <> nil) and not Collection.IsDesigning;
end;

procedure TdxNavBarDefaultStyle.Changed(AType: TdxNavBarChangeType);
begin
  inherited;
  DefaultValues := FAssignedValues = [];
  if Collection <> nil then
    Collection.OnStyleChange(Self, AType);
end;

procedure TdxNavBarDefaultStyle.ChangeScale(M, D: Integer);
begin
  if DefaultValues then
  begin
    inherited;
    FAssignedValues := [];
    FDefaultValues := True;
  end
  else
    inherited;
end;

procedure TdxNavBarDefaultStyle.AssignDefaultValues(CheckDefault: Boolean);
begin
  if Assigned(FDefaultValuesProc) and (DefaultValues or not CheckDefault) then
  begin
    FDefaultValuesProc;
    FDefaultValues := True;
    FAssignedValues := [];
  end;
end;

function TdxNavBarDefaultStyle.GetCollection: TdxNavBarDefaultStyles;
begin
  if Owner is TdxNavBarDefaultStyles then
    Result := TdxNavBarDefaultStyles(Owner)
  else
    Result := nil;
end;

{ TdxNavBarDefaultStyles }

constructor TdxNavBarDefaultStyles.Create(AOwner: TPersistent);
begin
  inherited;
  FUpdateCount := 0;
  FDefaultStylesList := TList.Create;
  CreateDefaultStyleList;
end;

destructor TdxNavBarDefaultStyles.Destroy;
begin
  DestroyDefaultStyleList;
  FDefaultStylesList.Free;
  inherited Destroy;
end;

procedure TdxNavBarDefaultStyles.Assign(Source: TPersistent);
var
  I: Integer;
  AStyles: TdxNavBarDefaultStyles;
begin
  if Source is TdxNavBarDefaultStyles then
  begin
    AStyles := Source as TdxNavBarDefaultStyles;
    for I := 0 to DefaultStyleCount - 1 do
      if I < AStyles.DefaultStyleCount then
        DefaultStyles[I].Assign(AStyles.DefaultStyles[I]);
  end
  else inherited;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultValues(CheckDefault: Boolean);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to DefaultStyleCount - 1 do
      DefaultStyles[I].AssignDefaultValues(CheckDefault);
  finally
    EndUpdate;
  end;
end;

procedure TdxNavBarDefaultStyles.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxNavBarDefaultStyles.EndUpdate;
begin
  Dec(FUpdateCount);
end;

procedure TdxNavBarDefaultStyles.AssignDefaultBackgroundStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultBackgroundStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultButtonStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultButtonStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultButtonPressedStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultButtonPressedStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultButtonHotTrackedStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultButtonHotTrackedStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultGroupBackgroundStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultGroupBackgroundStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultGroupControlStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultGroupControlStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultGroupHeaderStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultGroupHeaderStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultGroupHeaderActiveStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultGroupHeaderActiveStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultGroupHeaderActiveHotTrackedStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultGroupHeaderActiveHotTrackedStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultGroupHeaderActivePressedStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultGroupHeaderActivePressedStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultGroupHeaderHotTrackedStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultGroupHeaderHotTrackedStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultGroupHeaderPressedStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultGroupHeaderPressedStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultHintStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultHintStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultItemStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultItemStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultItemDisabledStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultItemDisabledStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultItemHotTrackedStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultItemHotTrackedStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultItemPressedStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultItemPressedStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultDropTargetGroupStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultDropTargetGroupStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultDropTargetLinkStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultDropTargetLinkStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultNavigationPaneHeaderStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultNavigationPaneHeaderStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultChildGroupCaptionHotTrackedStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultChildGroupCaptionHotTrackedStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultChildGroupCaptionPressedStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultChildGroupCaptionPressedStyle;
end;

procedure TdxNavBarDefaultStyles.AssignDefaultChildGroupCaptionStyle;
begin
  TdxCustomNavBar(Owner).ViewInfo.AssignDefaultChildGroupCaptionStyle;
end;

procedure TdxNavBarDefaultStyles.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to DefaultStyleCount - 1 do
      DefaultStyles[I].ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

procedure TdxNavBarDefaultStyles.CreateDefaultStyleList;

  procedure AddStyle(ADefaultValuesProc: TdxNavBarDefaultStyleAssignProc);
  var
    AStyle: TdxNavBarDefaultStyle;
  begin
    AStyle := TdxNavBarDefaultStyle.Create(Self);
    FDefaultStylesList.Add(AStyle);
    AStyle.FDefaultValuesProc := ADefaultValuesProc;
  end;

begin
  AddStyle(AssignDefaultBackgroundStyle);
  AddStyle(AssignDefaultButtonStyle);
  AddStyle(AssignDefaultButtonPressedStyle);
  AddStyle(AssignDefaultButtonHotTrackedStyle);
  AddStyle(AssignDefaultGroupBackgroundStyle);
  AddStyle(AssignDefaultGroupControlStyle);
  AddStyle(AssignDefaultGroupHeaderStyle);
  AddStyle(AssignDefaultGroupHeaderActiveStyle);
  AddStyle(AssignDefaultGroupHeaderActiveHotTrackedStyle);
  AddStyle(AssignDefaultGroupHeaderActivePressedStyle);
  AddStyle(AssignDefaultGroupHeaderHotTrackedStyle);
  AddStyle(AssignDefaultGroupHeaderPressedStyle);
  AddStyle(AssignDefaultHintStyle);
  AddStyle(AssignDefaultItemStyle);
  AddStyle(AssignDefaultItemDisabledStyle);
  AddStyle(AssignDefaultItemHotTrackedStyle);
  AddStyle(AssignDefaultItemPressedStyle);
  AddStyle(AssignDefaultDropTargetGroupStyle);
  AddStyle(AssignDefaultDropTargetLinkStyle);
  AddStyle(AssignDefaultNavigationPaneHeaderStyle);
  AddStyle(AssignDefaultChildGroupCaptionStyle);
  AddStyle(AssignDefaultChildGroupCaptionHotTrackedStyle);
  AddStyle(AssignDefaultChildGroupCaptionPressedStyle);
end;

procedure TdxNavBarDefaultStyles.DestroyDefaultStyleList;
var
  I: Integer;
begin
  for I := 0 to DefaultStyleCount - 1 do
    DefaultStyles[I].Free;
  FDefaultStylesList.Clear;
end;

procedure TdxNavBarDefaultStyles.OnStyleChange(Sender: TObject; AType: TdxNavBarChangeType);
begin
  if FUpdateCount = 0 then
  begin
    AssignDefaultValues(True);
    if Assigned(FOnChange) then FOnChange(Self, AType);
  end;
end;

function TdxNavBarDefaultStyles.IsDefaultStyleStored(Index: Integer): Boolean;
begin
  Result := not DefaultStyles[Index].DefaultValues;
end;

function TdxNavBarDefaultStyles.GetDefaultStyleCount: Integer;
begin
  Result := FDefaultStylesList.Count;
end;

function TdxNavBarDefaultStyles.GetDefaultStyle(Index: Integer): TdxNavBarDefaultStyle;
begin
  Result := TdxNavBarDefaultStyle(FDefaultStylesList[Index]);
end;

function TdxNavBarDefaultStyles.GetIsDesigning: Boolean;
begin
  Result := (Owner is TComponent) and (csDesigning in TComponent(Owner).ComponentState);
end;

procedure TdxNavBarDefaultStyles.SetDefaultStyle(Index: Integer; const Value: TdxNavBarDefaultStyle);
begin
  TdxNavBarDefaultStyle(FDefaultStylesList[Index]).Assign(Value);
end;

{ TdxBarStyleItem }

constructor TdxNavBarStyleItem.Create(AOwner: TComponent);
begin
  inherited;
  FChangeStyleHandlers := TcxEventHandlerCollection.Create;
  FStyle := TdxNavBarStyle.Create(Self);
  FStyle.OnChange := OnStyleChange;
end;

destructor TdxNavBarStyleItem.Destroy;
begin
  FreeAndNil(FStyle);
  FreeAndNil(FChangeStyleHandlers);
  inherited Destroy;
end;

procedure TdxNavBarStyleItem.Assign(Source: TPersistent);
begin
  if Source is TdxNavBarStyleItem then
    Style := TdxNavBarStyleItem(Source).Style
  else
    inherited;
end;

procedure TdxNavBarStyleItem.ChangeScale(M, D: Integer);
begin
  Style.ChangeScale(M, D);
end;

function TdxNavBarStyleItem.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxCustomNavBar).Styles;
end;

procedure TdxNavBarStyleItem.OnStyleChange(Sender: TObject; AType: TdxNavBarChangeType);
begin
  if FChangeStyleHandlers <> nil then
    FChangeStyleHandlers.CallEvents(Self, AType);
end;

procedure TdxNavBarStyleItem.RegisterChanges(AValue: TcxEventHandler);
begin
  if FChangeStyleHandlers <> nil then
    FChangeStyleHandlers.Add(AValue);
end;

procedure TdxNavBarStyleItem.UnRegisterChanges(AValue: TcxEventHandler);
begin
  if FChangeStyleHandlers <> nil then
    FChangeStyleHandlers.Remove(AValue);
end;

procedure TdxNavBarStyleItem.SetStyle(const Value: TdxNavBarStyle);
begin
  FStyle.Assign(Value);
end;

{ TdxBarStyles }

function TdxNavBarStyleRepository.Add: TdxNavBarStyleItem;
begin
  Result := inherited Add as TdxNavBarStyleItem;
end;

procedure TdxNavBarStyleRepository.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].ChangeScale(M, D);
  finally
    EndUpdate
  end;
end;

procedure TdxNavBarStyleRepository.SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);
begin
  if TdxCustomNavBarAccess(ParentComponent).FDesignHelper <> nil then
    AItem.Name := TdxCustomNavBarAccess(ParentComponent).FDesignHelper.UniqueName(ParentComponent,
      TdxCustomNavBarAccess(ParentComponent).Name + 'StyleItem')
  else
    inherited;
end;

function TdxNavBarStyleRepository.GetItem(Index: Integer): TdxNavBarStyleItem;
begin
  Result := inherited GetItem(Index) as TdxNavBarStyleItem;
end;

procedure TdxNavBarStyleRepository.SetItem(Index: Integer; Value: TdxNavBarStyleItem);
begin
  inherited SetItem(Index, Value);
end;

{ TdxNavBarBaseCustomStyles }

constructor TdxNavBarBaseCustomStyles.Create(AOwner: TPersistent);
begin
  inherited;
  SetLength(FStyles, GetStyleCount);
end;

destructor TdxNavBarBaseCustomStyles.Destroy;
var
  I: Integer;
begin
  for I := 0 to GetStyleCount - 1 do
    Styles[I] := nil;
  inherited Destroy;
end;

procedure TdxNavBarBaseCustomStyles.Assign(Source: TPersistent);
var
  I: Integer;
  ASourceStyles: TdxNavBarBaseCustomStyles;
begin
  if Source is TdxNavBarBaseCustomStyles then
  begin
    ASourceStyles := TdxNavBarBaseCustomStyles(Source);
    for I := 0 to GetStyleCount - 1 do
      if I < ASourceStyles.GetStyleCount then
        Styles[I] := ASourceStyles.Styles[I]
      else
        Styles[I] := nil;
  end
  else
    inherited;
end;

procedure TdxNavBarBaseCustomStyles.AddNotificator(AIndex: Integer);
begin
  if Styles[AIndex] <> nil then
    Styles[AIndex].RegisterChanges(InternalStyleChange);
end;

procedure TdxNavBarBaseCustomStyles.DoChange;
begin
  if Assigned(FOnChange) then
    OnChange(Self, doRecreate);
end;

function TdxNavBarBaseCustomStyles.GetStyle(Index: Integer): TdxNavBarStyleItem;
begin
  Result := FStyles[Index];
end;

procedure TdxNavBarBaseCustomStyles.SetStyle(Index: Integer; const Value: TdxNavBarStyleItem);
begin
  if FStyles[Index] <> Value then
  begin
    RemoveNotificator(Index);
    FNotifyComponent.RemoveSender(FStyles[Index]);
    FStyles[Index] := Value;
    FNotifyComponent.AddSender(FStyles[Index]);
    AddNotificator(Index);
    DoChange;
  end;
end;

procedure TdxNavBarBaseCustomStyles.FreeNotification(AComponent: TComponent);
var
  I: Integer;
begin
  for I := 0 to GetStyleCount - 1 do
    if Styles[I] = AComponent then
      Styles[I] := nil;
end;

procedure TdxNavBarBaseCustomStyles.RemoveNotificator(AIndex: Integer);
begin
  if Styles[AIndex] <> nil then
    Styles[AIndex].UnRegisterChanges(InternalStyleChange);
end;

procedure TdxNavBarBaseCustomStyles.InternalStyleChange(Sender: TObject; const AEventArgs);
begin
  DoChange;
end;

{ TdxNavBarCustomStyles }

function TdxNavBarCustomStyles.GetStyleCount: Integer;
begin
  Result := 8; //TODO: constant
end;

{ TdxNavBarGroupCustomStyles }

function TdxNavBarGroupCustomStyles.GetStyleCount: Integer;
begin
  Result := 11; //TODO: constant
end;

{ TdxNavBarItemCustomStyles }

function TdxNavBarItemCustomStyles.GetStyleCount: Integer;
begin
  Result := 4;
end;

{ TdxNavBarStyleOptions }

constructor TdxNavBarStyleOptions.Create(AOwner: TPersistent);
begin
  inherited;
  FCustomStyleRepository := TdxNavBarStyleRepository.Create(AOwner as TComponent, TdxNavBarStyleItem);
  FCustomStyleRepository.OnChange := CustomStylesChanged;
  FDefaultStyles := TdxNavBarDefaultStyles.Create(AOwner);
  FDefaultStyles.OnChange := StyleChanged;
  FCustomStyles := TdxNavBarCustomStyles.Create(AOwner);
  FCustomStyles.OnChange := StyleChanged;
end;

destructor TdxNavBarStyleOptions.Destroy;
begin
  FreeAndNil(FCustomStyles);
  FreeAndNil(FDefaultStyles);
  FreeAndNil(FCustomStyleRepository);
  inherited;
end;

procedure TdxNavBarStyleOptions.Assign(Source: TPersistent);
var
  ASourceStyleOptions: TdxNavBarStyleOptions;
begin
  if Source is TdxNavBarStyleOptions then
  begin
    ASourceStyleOptions := TdxNavBarStyleOptions(Source);
    CustomStyleRepository := ASourceStyleOptions.CustomStyleRepository;
    DefaultStyles := ASourceStyleOptions.DefaultStyles;
    CustomStyles := ASourceStyleOptions.CustomStyles;
    OnChange := ASourceStyleOptions.OnChange;
  end
  else
    inherited;
end;

procedure TdxNavBarStyleOptions.ChangeScale(M, D: Integer);
begin
  DefaultStyles.ChangeScale(M, D);
  CustomStyleRepository.ChangeScale(M, D);
end;

procedure TdxNavBarStyleOptions.CustomStylesChanged(Sender: TObject;
  AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification);
begin
  CallNotify(OnChange, Self);
end;

procedure TdxNavBarStyleOptions.StyleChanged(Sender: TObject; AType: TdxNavBarChangeType);
begin
  CallNotify(OnChange, Self);
end;

procedure TdxNavBarStyleOptions.SetCustomStyleRepository(Value: TdxNavBarStyleRepository);
begin
  FCustomStyleRepository.Assign(Value);
end;

procedure TdxNavBarStyleOptions.SetDefaultStyles(Value: TdxNavBarDefaultStyles);
begin
  FDefaultStyles.Assign(Value);
end;

procedure TdxNavBarStyleOptions.SetCustomStyles(Value: TdxNavBarCustomStyles);
begin
  FCustomStyles.Assign(Value);
end;

initialization
  RegisterClasses([TdxNavBarStyleItem]);

end.
