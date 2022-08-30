{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetCellStyleEditDialogController;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, cxLookAndFeelPainters, dxCore, cxGeometry,
  dxSpreadSheetCoreStyles, dxSpreadSheetNumberFormat, dxSpreadSheetStyles;

type
  TdxSpreadSheetCellStyleEditDialogCapability = (csecNumber, csecAlignment, csecFont, csecBorder, csecFill, csecProtection);
  TdxSpreadSheetCellStyleEditDialogCapabilities = set of TdxSpreadSheetCellStyleEditDialogCapability;

  { TdxSpreadSheetCellStyleEditDialogCustomController }

  TdxSpreadSheetCellStyleEditDialogCustomController = class
  strict private
    function GetDefaultFont: TdxSpreadSheetFontHandle;
    function GetPredefinedFormats: TdxSpreadSheetPredefinedFormats;
  protected
    FCapabilities: TdxSpreadSheetCellStyleEditDialogCapabilities;

    function GetCellStyles: TdxSpreadSheetCellStyles; virtual; abstract;
  public
    constructor Create;
    procedure EnumCellStyles(AProcRef: TdxSpreadSheetCellStyleEnumProcRef; AEnumDefaultStyles: Boolean); virtual; abstract;
    function FormatFocusedCellValue(const AFormatCode: string): TdxSpreadSheetNumberFormatResult; virtual;
    function HasMultipleCellsAreaAtHorz: Boolean; virtual;
    function HasMultipleCellsAreaAtVert: Boolean; virtual;

    // Merging
    function CanMergeAreas: Boolean; virtual;
    function GetMergeAreasInfo: TcxCheckBoxState; virtual;
    procedure MergeAreas; virtual;
    procedure UnmergeAreas; virtual;

    // Saving
    procedure BeginSaving; virtual;
    procedure EndSaving; virtual;
    procedure PrepareToSave; virtual;

    property Capabilities: TdxSpreadSheetCellStyleEditDialogCapabilities read FCapabilities;
    property CellStyles: TdxSpreadSheetCellStyles read GetCellStyles;
    property DefaultFont: TdxSpreadSheetFontHandle read GetDefaultFont;
    property PredefinedFormats: TdxSpreadSheetPredefinedFormats read GetPredefinedFormats;
  end;

  { TdxSpreadSheetCellStyleEditDialogSingleStyleController }

  TdxSpreadSheetCellStyleEditDialogSingleStyleController = class(TdxSpreadSheetCellStyleEditDialogCustomController)
  strict private
    FCellStyles: TdxSpreadSheetCellStyles;
    FStyle: TdxSpreadSheetCellStyle;
  protected
    function GetCellStyles: TdxSpreadSheetCellStyles; override;
  public
    constructor Create(ACellStyles: TdxSpreadSheetCellStyles; AStyle: TdxSpreadSheetCellStyle);
    procedure EnumCellStyles(AProcRef: TdxSpreadSheetCellStyleEnumProcRef; AEnumDefaultStyles: Boolean); override;
  end;

implementation

{ TdxSpreadSheetCellStyleEditDialogCustomController }

constructor TdxSpreadSheetCellStyleEditDialogCustomController.Create;
begin
  inherited Create;
  FCapabilities := [csecNumber..csecProtection];
end;

function TdxSpreadSheetCellStyleEditDialogCustomController.FormatFocusedCellValue(
  const AFormatCode: string): TdxSpreadSheetNumberFormatResult;
begin
  Result.Reset;
end;

function TdxSpreadSheetCellStyleEditDialogCustomController.CanMergeAreas: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCellStyleEditDialogCustomController.GetMergeAreasInfo: TcxCheckBoxState;
begin
  Result := cbsUnchecked;
end;

function TdxSpreadSheetCellStyleEditDialogCustomController.HasMultipleCellsAreaAtHorz: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCellStyleEditDialogCustomController.HasMultipleCellsAreaAtVert: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetCellStyleEditDialogCustomController.MergeAreas;
begin
  // do nothing
end;

procedure TdxSpreadSheetCellStyleEditDialogCustomController.UnmergeAreas;
begin
  // do nothing
end;

procedure TdxSpreadSheetCellStyleEditDialogCustomController.BeginSaving;
begin
  // do nothing
end;

procedure TdxSpreadSheetCellStyleEditDialogCustomController.EndSaving;
begin
  // do nothing
end;

procedure TdxSpreadSheetCellStyleEditDialogCustomController.PrepareToSave;
begin
  // do nothing
end;

function TdxSpreadSheetCellStyleEditDialogCustomController.GetDefaultFont: TdxSpreadSheetFontHandle;
begin
  Result := CellStyles.DefaultStyle.Font;
end;

function TdxSpreadSheetCellStyleEditDialogCustomController.GetPredefinedFormats: TdxSpreadSheetPredefinedFormats;
begin
  Result := CellStyles.Formats.PredefinedFormats;
end;

{ TdxSpreadSheetCellStyleEditDialogSingleStyleController }

constructor TdxSpreadSheetCellStyleEditDialogSingleStyleController.Create(
  ACellStyles: TdxSpreadSheetCellStyles; AStyle: TdxSpreadSheetCellStyle);
begin
  inherited Create;
  FStyle := AStyle;
  FCellStyles := ACellStyles;
  FCapabilities := [csecNumber, csecFont, csecBorder, csecFill];
end;

procedure TdxSpreadSheetCellStyleEditDialogSingleStyleController.EnumCellStyles(
  AProcRef: TdxSpreadSheetCellStyleEnumProcRef; AEnumDefaultStyles: Boolean);
begin
  AProcRef(FStyle, 0, 0, cxNullRect);
end;

function TdxSpreadSheetCellStyleEditDialogSingleStyleController.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := FCellStyles;
end;

end.
