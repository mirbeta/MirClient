{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Platform.Win.Scroll;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Windows, Messages, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections, Controls,
  StdCtrls, Forms, ActiveX,
  dxCoreClasses, cxGeometry, cxLookAndFeels, cxGraphics, cxControls, cxClasses, cxScrollBar,

  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.InnerControl,
  dxRichEdit.View.Core,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Platform.Font;

type


  { TdxWinFormsScrollBarAdapter }

  TdxWinFormsScrollBarAdapter = class(TInterfacedObject, IdxPlatformSpecificScrollBarAdapter)
  public
    procedure OnScroll(AAdapter: TdxScrollBarAdapter; ASender: TObject; E: TdxScrollEventArgs); virtual;
    procedure ApplyValuesToScrollBarCore(AAdapter: TdxScrollBarAdapter); virtual;
    function GetRawScrollBarValue(AAdapter: TdxScrollBarAdapter): Integer;
    function SetRawScrollBarValue(AAdapter: TdxScrollBarAdapter; Value: Integer): Boolean;
    function GetPageUpRawScrollBarValue(AAdapter: TdxScrollBarAdapter): Integer;
    function GetPageDownRawScrollBarValue(AAdapter: TdxScrollBarAdapter): Integer;
    function CreateLastScrollEventArgs(AAdapter: TdxScrollBarAdapter): TdxScrollEventArgs; virtual;
  end;

  { TdxWinFormsRichEditViewVerticalScrollController }

  TdxWinFormsRichEditViewVerticalScrollController = class(TdxRichEditViewVerticalScrollController)
  protected
    function UpdatePageNumberOnScroll(E: TdxScrollEventArgs): Boolean; override;
    function IsScrollTypeValid(E: TdxScrollEventArgs): Boolean; override;
    function CalculateScrollDelta(E: TdxScrollEventArgs): Integer; override;
    procedure ApplyNewScrollValue(Value: Integer); override;
    procedure ApplyNewScrollValueToScrollEventArgs(E: TdxScrollEventArgs; Value: Integer); override;
  end;

  { TdxWinFormsRichEditViewHorizontalScrollController }

  TdxWinFormsRichEditViewHorizontalScrollController = class(TdxRichEditViewHorizontalScrollController)
  protected
    procedure OnScrollCore(E: TdxScrollEventArgs); override;
  end;

  TdxOfficeScrollbar = class(TcxScrollBar, IdxOfficeScrollBar)
  protected type
    TData = record
      Enabled: Boolean;
      LargeChange: Integer;
      Maximum: Integer;
      Minimum: Integer;
      SmallChange: Integer;
      Value: Integer;
    end;
  strict private
    FData: TData;
    FLockCount: Integer;
    FScroll: TdxScrollEventHandler;
    function GetPosition: Integer;
    procedure SetPosition(Value: Integer);
  strict protected
    function IdxOfficeScrollBar.GetEnabled = IGetEnabled;
    procedure IdxOfficeScrollBar.SetEnabled = ISetEnabled;
    function IdxOfficeScrollBar.GetLargeChange = IGetLargeChange;
    procedure IdxOfficeScrollBar.SetLargeChange = ISetLargeChange;
    function IdxOfficeScrollBar.GetMaximum = IGetMaximum;
    procedure IdxOfficeScrollBar.SetMaximum = ISetMaximum;
    function IdxOfficeScrollBar.GetMinimum = IGetMinimum;
    procedure IdxOfficeScrollBar.SetMinimum = ISetMinimum;
    function IdxOfficeScrollBar.GetSmallChange = IGetSmallChange;
    procedure IdxOfficeScrollBar.SetSmallChange = ISetSmallChange;
    function IdxOfficeScrollBar.GetValue = IGetValue;
    procedure IdxOfficeScrollBar.SetValue = ISetValue;
    function IdxOfficeScrollBar.GetScroll = IGetScroll;
    function IGetScroll: TdxScrollEventHandler;

    procedure BeginUpdate;
    procedure EndUpdate;

    function IGetEnabled: Boolean;
    procedure ISetEnabled(Value: Boolean);
    function IGetLargeChange: Integer;
    procedure ISetLargeChange(Value: Integer);
    function IGetMaximum: Integer;
    procedure ISetMaximum(Value: Integer);
    function IGetMinimum: Integer;
    procedure ISetMinimum(Value: Integer);
    function IGetSmallChange: Integer;
    procedure ISetSmallChange(Value: Integer);
    function IGetValue: Integer;
    procedure ISetValue(Value: Integer);

    procedure Change; override;
    procedure ExecuteScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer); virtual;
  protected
    function GetHelperClass: TcxScrollBarHelperClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    function IsOverlapScrollBar: Boolean; virtual;
  published
    property Position: Integer read GetPosition write SetPosition default 0;
  end;

implementation

uses
  Math;

type

  TdxOfficeScrollBarHelper = class(TcxScrollBarHelper)
  protected
    function GetControllerClass: TcxScrollBarControllerClass; override;
  end;

  TdxOfficeScrollBarController = class(TcxScrollBarController)
  protected
    procedure SetThumbnailValue(AValue: Integer; AUseSetter: Boolean); override;
  end;

{ TdxOfficeScrollBarHelper }

function TdxOfficeScrollBarHelper.GetControllerClass: TcxScrollBarControllerClass;
begin
  Result := TdxOfficeScrollBarController;
end;

{ TdxOfficeScrollBarController }

procedure TdxOfficeScrollBarController.SetThumbnailValue(AValue: Integer;
  AUseSetter: Boolean);
begin
  SetPositionValue(scTrack, AValue);
end;

{ TdxWinFormsScrollBarAdapter }

procedure TdxWinFormsScrollBarAdapter.OnScroll(AAdapter: TdxScrollBarAdapter; ASender: TObject;
  E: TdxScrollEventArgs);
var
  ADelta: Integer;
  AArgs: TdxScrollEventArgs;
begin
  ADelta := E.NewValue - AAdapter.GetRawScrollBarValue;
  if AAdapter.EnsureSynchronizedCore then
  begin
    AArgs := TdxScrollEventArgs.Create(E.&Type, AAdapter.GetRawScrollBarValue, AAdapter.GetRawScrollBarValue + ADelta, E.ScrollOrientation);
    try
      AAdapter.RaiseScroll(AArgs);
      E.NewValue := AArgs.NewValue;
    finally
      AArgs.Free;
    end;
  end
  else
    AAdapter.RaiseScroll(E);
end;

procedure TdxWinFormsScrollBarAdapter.ApplyValuesToScrollBarCore(AAdapter: TdxScrollBarAdapter);
begin
  if AAdapter.Maximum > High(TScrollBarInc) then
    AAdapter.Factor := 1.0 / (1 + (AAdapter.Maximum / High(TScrollBarInc)))
  else
    AAdapter.Factor := 1.0;

  AAdapter.ScrollBar.BeginUpdate;
  try
    AAdapter.ScrollBar.Minimum := Round(AAdapter.Factor * AAdapter.Minimum);
    AAdapter.ScrollBar.Maximum := Round(AAdapter.Factor * AAdapter.Maximum);
    AAdapter.ScrollBar.LargeChange := Round(AAdapter.Factor * AAdapter.LargeChange);
    AAdapter.ScrollBar.SmallChange := Min(AAdapter.ScrollBar.LargeChange, Round(AAdapter.Factor * AAdapter.SmallChange));
    AAdapter.ScrollBar.Value := Round(AAdapter.Factor * AAdapter.Value);
    AAdapter.ScrollBar.Enabled := AAdapter.Enabled;
  finally
    AAdapter.ScrollBar.EndUpdate;
  end;
end;

function TdxWinFormsScrollBarAdapter.GetRawScrollBarValue(AAdapter: TdxScrollBarAdapter): Integer;
begin
  Result := AAdapter.ScrollBar.Value;
end;

function TdxWinFormsScrollBarAdapter.SetRawScrollBarValue(AAdapter: TdxScrollBarAdapter; Value: Integer): Boolean;
begin
  Result := AAdapter.ScrollBar.Value <> Value;
  if Result then
  begin
    AAdapter.ScrollBar.Value := Value;
    AAdapter.Value := Round(Value / AAdapter.Factor);
  end;
end;

function TdxWinFormsScrollBarAdapter.CreateLastScrollEventArgs(
  AAdapter: TdxScrollBarAdapter): TdxScrollEventArgs;
begin
  Result := TdxScrollEventArgs.Create(TdxScrollEventType.Last, AAdapter.ScrollBar.Maximum - AAdapter.ScrollBar.LargeChange);
end;

function TdxWinFormsScrollBarAdapter.GetPageUpRawScrollBarValue(AAdapter: TdxScrollBarAdapter): Integer;
begin
  Result := Max(AAdapter.ScrollBar.Minimum, AAdapter.ScrollBar.Value - AAdapter.ScrollBar.LargeChange);
end;

function TdxWinFormsScrollBarAdapter.GetPageDownRawScrollBarValue(AAdapter: TdxScrollBarAdapter): Integer;
begin
  Result := Min(AAdapter.ScrollBar.Maximum - AAdapter.ScrollBar.LargeChange + 1, AAdapter.ScrollBar.Value + AAdapter.ScrollBar.LargeChange);
end;

{ TdxWinFormsRichEditViewVerticalScrollController }

procedure TdxWinFormsRichEditViewVerticalScrollController.ApplyNewScrollValue(
  Value: Integer);
begin
  ScrollBarAdapter.SetRawScrollBarValue(Value);
end;

procedure TdxWinFormsRichEditViewVerticalScrollController.ApplyNewScrollValueToScrollEventArgs(
  E: TdxScrollEventArgs; Value: Integer);
begin
  E.NewValue := Value;
end;

function TdxWinFormsRichEditViewVerticalScrollController.CalculateScrollDelta(
  E: TdxScrollEventArgs): Integer;
begin
  Result := E.NewValue - ScrollBarAdapter.GetRawScrollBarValue;
end;

function TdxWinFormsRichEditViewVerticalScrollController.IsScrollTypeValid(
  E: TdxScrollEventArgs): Boolean;
begin
  Result := E.&Type <> TdxScrollEventType.EndScroll;
end;

function TdxWinFormsRichEditViewVerticalScrollController.UpdatePageNumberOnScroll(
  E: TdxScrollEventArgs): Boolean;
begin
  Result := False;
end;

{ TdxWinFormsRichEditViewHorizontalScrollController }

procedure TdxWinFormsRichEditViewHorizontalScrollController.OnScrollCore(
  E: TdxScrollEventArgs);
begin
  ScrollBarAdapter.Value := E.NewValue;
end;

{ TdxOfficeScrollbar }

constructor TdxOfficeScrollbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csFramed];
  OnScroll := ExecuteScroll;
end;

function TdxOfficeScrollbar.IsOverlapScrollBar: Boolean;
begin
  Result := False;
end;

procedure TdxOfficeScrollbar.BeginUpdate;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
  begin
    FData.Enabled := Enabled;
    FData.LargeChange := LargeChange;
    FData.Maximum := Max;
    FData.Minimum := Min;
    FData.SmallChange := SmallChange;
    FData.Value := Position;
  end;
end;

procedure TdxOfficeScrollbar.Change;
begin
  if FLockCount = 0 then
    inherited Change;
end;

procedure TdxOfficeScrollbar.EndUpdate;
begin
  if FLockCount = 1 then
  begin
    if (Position <> FData.Value) or
       (LargeChange <> FData.LargeChange) or
       (SmallChange <> FData.SmallChange) or
       (Max <> FData.Maximum) or
       (Min <> FData.Minimum) or
       (Enabled <> FData.Enabled) then
    begin
      if LargeChange <> FData.LargeChange then
        inherited LargeChange := FData.LargeChange;
      if SmallChange <> FData.SmallChange then
        inherited SmallChange := FData.SmallChange;
      SetScrollParams(FData.Minimum, FData.Maximum, FData.Value,
        Math.Min(FData.LargeChange, FData.Maximum - FData.Minimum){PageSize},
        False);
      if Enabled <> FData.Enabled then
        inherited Enabled := FData.Enabled;
      if HandleAllocated then
        Repaint;
    end;
  end;
  Dec(FLockCount);
end;

function TdxOfficeScrollbar.IGetEnabled: Boolean;
begin
  Result := Enabled
end;

function TdxOfficeScrollbar.IGetLargeChange: Integer;
begin
  Result := LargeChange
end;

function TdxOfficeScrollbar.IGetMaximum: Integer;
begin
  Result := Max
end;

function TdxOfficeScrollbar.IGetMinimum: Integer;
begin
  Result := Min
end;

function TdxOfficeScrollbar.IGetScroll: TdxScrollEventHandler;
begin
  Result := FScroll.Clone;
end;

function TdxOfficeScrollbar.IGetSmallChange: Integer;
begin
  Result := SmallChange
end;

function TdxOfficeScrollbar.IGetValue: Integer;
begin
  Result := Position;
end;

procedure TdxOfficeScrollbar.ISetEnabled(Value: Boolean);
begin
  if FLockCount = 0 then
    Enabled := Value
  else
    FData.Enabled := Value;
end;

procedure TdxOfficeScrollbar.ISetLargeChange(Value: Integer);
begin
  if FLockCount = 0 then
  begin
    LargeChange := Value;
    PageSize := Math.Min(FData.LargeChange, Max - Min);
  end
  else
    FData.LargeChange := Value;
end;

procedure TdxOfficeScrollbar.ISetMaximum(Value: Integer);
begin
  if FLockCount = 0 then
    Max := Value
  else
    FData.Maximum := Value;
end;

procedure TdxOfficeScrollbar.ISetMinimum(Value: Integer);
begin
  if FLockCount = 0 then
    Min := Value
  else
    FData.Minimum := Value;
end;

procedure TdxOfficeScrollbar.ISetSmallChange(Value: Integer);
begin
  if FLockCount = 0 then
    SmallChange := Value
  else
    FData.SmallChange := Value;
end;

procedure TdxOfficeScrollbar.ISetValue(Value: Integer);
begin
  if FLockCount = 0 then
    inherited Position := Value
  else
    FData.Value := Value;
end;

function ScrollCodeToScrollEventType(AScrollCode: TScrollCode): TdxScrollEventType;
const
  Map: array[TScrollCode] of TdxScrollEventType = (
    TdxScrollEventType.SmallDecrement, TdxScrollEventType.SmallIncrement, TdxScrollEventType.LargeDecrement,
    TdxScrollEventType.LargeIncrement, TdxScrollEventType.ThumbPosition, TdxScrollEventType.ThumbTrack,
    TdxScrollEventType.First, TdxScrollEventType.Last, TdxScrollEventType.EndScroll);
begin
  Result := Map[AScrollCode];
end;

function ScrollBarKindToScrollOrientation(AKind: TScrollBarKind): TdxScrollOrientation;
const
  Map: array[TScrollBarKind] of TdxScrollOrientation =
    (TdxScrollOrientation.HorizontalScroll, TdxScrollOrientation.VerticalScroll);
begin
  Result := Map[AKind];
end;

procedure TdxOfficeScrollbar.ExecuteScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  E: TdxScrollEventArgs;
begin
  if not FScroll.Empty then
  begin
    E := TdxScrollEventArgs.Create(ScrollCodeToScrollEventType(ScrollCode), ScrollPos, ScrollBarKindToScrollOrientation(Kind));
    try
      FScroll.Invoke(Sender, E);
    finally
      ScrollPos := E.NewValue;
      E.Free;
    end;
  end;
end;

function TdxOfficeScrollbar.GetHelperClass: TcxScrollBarHelperClass;
begin
  Result := TdxOfficeScrollBarHelper;
end;

function TdxOfficeScrollbar.GetPosition: Integer;
begin
  Result := inherited Position;
end;

procedure TdxOfficeScrollbar.SetPosition(Value: Integer);
begin
  ExecuteScroll(Self, scTrack, Value);
  inherited Position := Value;
end;

end.
