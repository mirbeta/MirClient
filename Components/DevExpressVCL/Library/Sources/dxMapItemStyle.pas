{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxMapItemStyle;

interface

{$I cxVer.inc}

uses
  Types, Classes, Graphics, SysUtils,
  dxCore, dxCoreClasses, dxCoreGraphics, cxGraphics, cxGeometry;

type
  TdxMapItemStyleValue = (mcsvColor, mcsvBorderWidth,
    mcsvBorderColor, mcsvFont, mcsvTextColor, mcsvTextGlowColor);
  TdxMapItemStyleValues = set of TdxMapItemStyleValue;

  TdxCustomMapItemStyle = class(TcxOwnedPersistent)
  private
    FAssignedValues: TdxMapItemStyleValues;
    FBorderColor: TdxAlphaColor;
    FBorderWidth: Integer;
    FColor: TdxAlphaColor;
    FFont: TFont;
    FTextColor: TdxAlphaColor;
    FTextGlowColor: TdxAlphaColor;
    FOnChanged: TNotifyEvent;
    function IsBorderColorStored: Boolean;
    function IsBorderWidthStored: Boolean;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    function IsTextColorStored: Boolean;
    function IsTextGlowColorStored: Boolean;
    procedure SetAssignedValues(const Value: TdxMapItemStyleValues);
    procedure SetBorderColor(const Value: TdxAlphaColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetColor(const Value: TdxAlphaColor);
    procedure SetFont(Value: TFont);
    procedure SetTextColor(const Value: TdxAlphaColor);
    procedure SetTextGlowColor(const Value: TdxAlphaColor);
  protected
    procedure Changed;
    procedure DoAssign(Source: TPersistent); override;
    procedure FontChanged(ASender: TObject);

    property AssignedValues: TdxMapItemStyleValues read FAssignedValues write SetAssignedValues default [];
    property BorderColor: TdxAlphaColor read FBorderColor write SetBorderColor stored IsBorderColorStored;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth stored IsBorderWidthStored;
    property Color: TdxAlphaColor read FColor write SetColor stored IsColorStored;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property TextColor: TdxAlphaColor read FTextColor write SetTextColor stored IsTextColorStored;
    property TextGlowColor: TdxAlphaColor read FTextGlowColor write SetTextGlowColor stored IsTextGlowColorStored;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TdxMapItemStyle = class(TdxCustomMapItemStyle)
  published
    property AssignedValues;
    property BorderColor;
    property BorderWidth;
    property Color;
    property Font;
    property TextColor;
    property TextGlowColor;
  end;

implementation

uses
  Windows, dxDPIAwareUtils;

{ TdxMapControlCustomMapItemStyle }

constructor TdxCustomMapItemStyle.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.Style := [fsBold];
  FFont.Height := dxGetFontHeightForDefaultDPI(8);
  FFont.OnChange := FontChanged;
  FBorderWidth := 1;
  FBorderColor := dxacDefault;
  FColor := dxacDefault;
  FTextColor := dxacDefault;
  FTextGlowColor := dxacDefault;
end;

destructor TdxCustomMapItemStyle.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TdxCustomMapItemStyle.Changed;
begin
  dxCallNotify(FOnChanged, Self);
end;

procedure TdxCustomMapItemStyle.DoAssign(Source: TPersistent);
var
  AMapItemStyle: TdxCustomMapItemStyle;
begin
  inherited;
  if Source is TdxCustomMapItemStyle then
  begin
    AMapItemStyle := TdxCustomMapItemStyle(Source);
    BorderColor := AMapItemStyle.BorderColor;
    BorderWidth := AMapItemStyle.BorderWidth;
    Color := AMapItemStyle.Color;
    Font := AMapItemStyle.Font;
    TextColor := AMapItemStyle.TextColor;
    TextGlowColor := AMapItemStyle.TextGlowColor;
  end;
end;

procedure TdxCustomMapItemStyle.FontChanged(ASender: TObject);
begin
  Include(FAssignedValues, mcsvFont);
  Changed;
end;

function TdxCustomMapItemStyle.IsBorderColorStored: Boolean;
begin
  Result := mcsvBorderColor in FAssignedValues;
end;

function TdxCustomMapItemStyle.IsBorderWidthStored: Boolean;
begin
  Result := mcsvBorderWidth in FAssignedValues;
end;

function TdxCustomMapItemStyle.IsColorStored: Boolean;
begin
  Result := mcsvColor in FAssignedValues;
end;

function TdxCustomMapItemStyle.IsFontStored: Boolean;
begin
  Result := mcsvFont in FAssignedValues;
end;

function TdxCustomMapItemStyle.IsTextColorStored: Boolean;
begin
  Result := mcsvTextColor in FAssignedValues;
end;

function TdxCustomMapItemStyle.IsTextGlowColorStored: Boolean;
begin
  Result := mcsvTextGlowColor in FAssignedValues;
end;

procedure TdxCustomMapItemStyle.SetAssignedValues(
  const Value: TdxMapItemStyleValues);
begin
  if FAssignedValues <> Value then
  begin
    FAssignedValues := Value;
    Changed;
  end;
end;

procedure TdxCustomMapItemStyle.SetBorderColor(const Value: TdxAlphaColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    if FBorderColor = dxacDefault then
      Exclude(FAssignedValues, mcsvBorderColor)
    else
      Include(FAssignedValues, mcsvBorderColor);
    Changed;
  end;
end;

procedure TdxCustomMapItemStyle.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    Include(FAssignedValues, mcsvBorderWidth);
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TdxCustomMapItemStyle.SetColor(const Value: TdxAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FColor = dxacDefault then
      Exclude(FAssignedValues, mcsvColor)
    else
      Include(FAssignedValues, mcsvColor);
    Changed;
  end;
end;

procedure TdxCustomMapItemStyle.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TdxCustomMapItemStyle.SetTextColor(const Value: TdxAlphaColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    if FTextColor = dxacDefault then
      Exclude(FAssignedValues, mcsvTextColor)
    else
      Include(FAssignedValues, mcsvTextColor);
    Changed;
  end;
end;

procedure TdxCustomMapItemStyle.SetTextGlowColor(const Value: TdxAlphaColor);
begin
  if FTextGlowColor <> Value then
  begin
    FTextGlowColor := Value;
    if FTextGlowColor = dxacDefault then
      Exclude(FAssignedValues, mcsvTextGlowColor)
    else
      Include(FAssignedValues, mcsvTextGlowColor);
    Changed;
  end;
end;

end.
