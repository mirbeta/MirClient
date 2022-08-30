{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSDBCtrlLnks;

interface

{$I cxVer.inc}

uses
  Graphics, DBCtrls, dxPSCore, dxPSGraphicLnk, dxPSLbxLnk, dxPSTextLnk,
  dxPSRELnk;

type
  TdxDBImageReportLink = class(TCustomdxPictureReportLink)
  private
    function GetDBImage: TDBImage;
  protected
    procedure InternalRestoreDefaults; override;
    procedure InternalRestoreFromOriginal; override;

    function GetGraphicHeight: Integer; override;
    function GetGraphicWidth: Integer; override;
    function GetPicture: TPicture; override;
  public
    function DataProviderPresent: Boolean; override;
    property DBImage: TDBImage read GetDBImage;
  published
    property BorderColor;
    property DrawBorder;
    property Transparent;
    property TransparentColor;
  end;

  TdxDBListBoxReportLink = class(TCustomdxListBoxReportLinkControl)
  private
    function GetDBListBox: TDBListBox;
  public
    property DBListBox: TDBListBox read GetDBListBox;
  published
    property Color;
    property DrawMode;
    property EndEllipsis;
    property EvenColor;
    property EvenFont;
    property Font;
    property OddColor;
    property OddFont;
    property Options;
    property Multiline;
    property PaintItemsGraphics;
    property RowAutoHeight;
    property ScaleFonts;
    property SupportedCustomDraw;
    property Transparent;
    property TransparentGraphics;
    property UseHorzDelimiters;
    property UseVertDelimiters;
    property Width;

    property OnCustomDrawItem;
    property OnInitializeItem;
  end;

  TdxDBRichEditReportLink = class(TCustomdxRichEditReportLink)
  private
    function GetDBRichEdit: TDBRichEdit;
  public
    property DBRichEdit: TDBRichEdit read GetDBRichEdit;
  end;

  TdxDBMemoReportLink = class(TdxCustomMemoReportLink)
  private
    function GetMemo: TDBMemo;
  public
    property Memo: TDBMemo read GetMemo;
  end;

implementation

{ TdxDBImageReportLink }

function TdxDBImageReportLink.DataProviderPresent: Boolean;
begin
  if DataSource = rldsComponent then
    Result := DBImage <> nil
  else
    Result := inherited DataProviderPresent;
end;

procedure TdxDBImageReportLink.InternalRestoreDefaults;
begin
  inherited;
  Center := True;
  Stretch := False;
end;

procedure TdxDBImageReportLink.InternalRestoreFromOriginal;
begin
  inherited;
  Center := DBImage.Center;
  Stretch := DBImage.Stretch;
end;

function TdxDBImageReportLink.GetGraphicHeight: Integer;
begin
  if DBImage <> nil then
  begin
    Result := DBImage.Height;
    if IsAggregated and (Result < inherited GetGraphicHeight) then
      Result := inherited GetGraphicHeight;
  end
  else
    Result := inherited GetGraphicHeight;
end;

function TdxDBImageReportLink.GetGraphicWidth: Integer;
begin
  if DBImage <> nil then
  begin
    Result := DBImage.Width;
    if IsAggregated and (Result < inherited GetGraphicWidth) then
      Result := inherited GetGraphicWidth;
  end
  else
    Result := inherited GetGraphicWidth;
end;

function TdxDBImageReportLink.GetPicture: TPicture;
begin
  if DBImage <> nil then
    Result := DBImage.Picture
  else
    Result := inherited GetPicture;
end;

function TdxDBImageReportLink.GetDBImage: TDBImage;
begin
  Result := inherited Component as TDBImage;
end;

{ TdxDBListBoxReportLink }

function TdxDBListBoxReportLink.GetDBListBox: TDBListBox;
begin
  Result := inherited Component as TDBListBox;
end;

{ TdxDBRichEditReportLink }

function TdxDBRichEditReportLink.GetDBRichEdit: TDBRichEdit;
begin
  Result := inherited Component as TDBRichEdit;
end;

{ TdxDBMemoReportLink }

function TdxDBMemoReportLink.GetMemo: TDBMemo;
begin
  Result := inherited Component as TDBMemo;
end;

initialization
  dxPSRegisterReportLink(TdxDBImageReportLink, TDBImage, nil);
  dxPSRegisterReportLink(TdxDBListBoxReportLink, TDBListBox, TdxLBxReportLinkDesignWindow);
  dxPSRegisterReportLink(TdxDBRichEditReportLink, TDBRichEdit, nil);
  dxPSRegisterReportLink(TdxDBMemoReportLink, TDBMemo, TdxfmTextReportLinkDesignWindow);

finalization
  dxPSUnregisterReportLink(TdxDBMemoReportLink, TDBMemo, TdxfmTextReportLinkDesignWindow);
  dxPSUnregisterReportLink(TdxDBRichEditReportLink, TDBRichEdit, nil);
  dxPSUnregisterReportLink(TdxDBListBoxReportLink, TDBListBox, TdxLBxReportLinkDesignWindow);
  dxPSUnregisterReportLink(TdxDBImageReportLink, TDBImage, nil);

end.
