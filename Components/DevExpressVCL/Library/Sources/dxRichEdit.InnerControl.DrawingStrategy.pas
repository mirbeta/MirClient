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

unit dxRichEdit.InnerControl.DrawingStrategy;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, dxCore,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.PieceTable;

type
  { TdxGdiPlusMeasurementAndDrawingStrategyBase }

  TdxGdiPlusMeasurementAndDrawingStrategyBase = class abstract(TdxMeasurementAndDrawingStrategy)
  strict private
    FGraphicsModifier: TdxGraphicsToLayoutUnitsModifier;
    FMeasureGraphics: TdxGraphics;
  protected
    function CreateMeasureGraphics: TdxGraphics; virtual;
  public
    destructor Destroy; override;

    procedure Initialize; override;
    procedure OnLayoutUnitChanged; override;

    property MeasureGraphics: TdxGraphics read FMeasureGraphics;
  end;

  { TdxGdiMeasurementAndDrawingStrategyBase }

  TdxGdiMeasurementAndDrawingStrategyBase = class(TdxGdiPlusMeasurementAndDrawingStrategyBase)
  strict private
    FHdcGraphicsModifier: TdxGraphicsToLayoutUnitsModifier;
    FHdcMeasureGraphics: TdxGraphics;
  protected
    property HdcMeasureGraphics: TdxGraphics read FHdcMeasureGraphics;
  public
    destructor Destroy; override;

    procedure Initialize; override;
    function CreateBoxMeasurer: TdxBoxMeasurer; override;
    procedure OnLayoutUnitChanged; override;
  end;

  { TdxServerGdiMeasurementAndDrawingStrategy }

  TdxServerGdiMeasurementAndDrawingStrategy = class(TdxGdiMeasurementAndDrawingStrategyBase)
  public
    function CreateDocumentPainter(AGraphics: TdxGraphics): TObject; override;
  end;

  { TdxGdiMeasurementAndDrawingStrategy }

  TdxGdiMeasurementAndDrawingStrategy = class(TdxGdiMeasurementAndDrawingStrategyBase)
  public
    function CreateDocumentPainter(AGraphics: TdxGraphics): TObject; override;
  end;

implementation

uses
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.Platform.Win.FontCache,
  dxRichEdit.LayoutEngine.BoxMeasurer;

{ TdxGdiPlusMeasurementAndDrawingStrategyBase }

destructor TdxGdiPlusMeasurementAndDrawingStrategyBase.Destroy;
begin
  FGraphicsModifier.Free;
  FMeasureGraphics.Free;
  inherited Destroy;
end;

procedure TdxGdiPlusMeasurementAndDrawingStrategyBase.Initialize;
begin
  FMeasureGraphics := CreateMeasureGraphics;
  inherited Initialize;
end;

procedure TdxGdiPlusMeasurementAndDrawingStrategyBase.OnLayoutUnitChanged;
begin
  FreeAndNil(FGraphicsModifier);
  FGraphicsModifier := TdxGraphicsToLayoutUnitsModifier.Create(FMeasureGraphics, DocumentModel.LayoutUnitConverter);
  inherited OnLayoutUnitChanged;
end;

function TdxGdiPlusMeasurementAndDrawingStrategyBase.CreateMeasureGraphics: TdxGraphics;
begin
  Result := TdxGraphics.Create;
  FGraphicsModifier.Free;
  FGraphicsModifier := TdxGraphicsToLayoutUnitsModifier.Create(Result, DocumentModel.LayoutUnitConverter);
end;

{ TdxGdiMeasurementAndDrawingStrategyBase }

destructor TdxGdiMeasurementAndDrawingStrategyBase.Destroy;
var
  AMeasurer: TdxGdiBoxMeasurerLockHdc;
begin
  AMeasurer := Safe<TdxGdiBoxMeasurerLockHdc>.Cast(Measurer);
  if AMeasurer <> nil then
    AMeasurer.ReleaseCachedHdc;

  FHdcGraphicsModifier.Free;
  FHdcMeasureGraphics.Free;
  inherited Destroy;
end;

procedure TdxGdiMeasurementAndDrawingStrategyBase.Initialize;
begin
  FHdcMeasureGraphics := TdxGraphics.Create;
  FHdcGraphicsModifier := TdxGraphicsToLayoutUnitsModifier.Create(FHdcMeasureGraphics, DocumentModel.LayoutUnitConverter);
  inherited Initialize;
end;

procedure TdxGdiMeasurementAndDrawingStrategyBase.OnLayoutUnitChanged;
var
  AMeasurer: TdxGdiBoxMeasurerLockHdc;
begin
  AMeasurer := Safe<TdxGdiBoxMeasurerLockHdc>.Cast(Measurer);
  if AMeasurer <> nil then
    AMeasurer.ReleaseCachedHdc;
  FHdcGraphicsModifier.Free;
  FHdcGraphicsModifier := TdxGraphicsToLayoutUnitsModifier.Create(FHdcMeasureGraphics, DocumentModel.LayoutUnitConverter);
  if AMeasurer <> nil then
    AMeasurer.ObtainCachedHdc;
  inherited OnLayoutUnitChanged;
end;

function TdxGdiMeasurementAndDrawingStrategyBase.CreateBoxMeasurer: TdxBoxMeasurer;
begin
  Result := TdxGdiBoxMeasurerLockHdc.Create(DocumentModel, MeasureGraphics, FHdcMeasureGraphics);
end;

{ TdxServerGdiMeasurementAndDrawingStrategy }

function TdxServerGdiMeasurementAndDrawingStrategy.CreateDocumentPainter(AGraphics: TdxGraphics): TObject;
begin
  Result := TdxEmptyPainter.Create(AGraphics);
end;

{ TdxGdiMeasurementAndDrawingStrategy }

function TdxGdiMeasurementAndDrawingStrategy.CreateDocumentPainter(
  AGraphics: TdxGraphics): TObject;
begin
  Result := TdxRichEditGdiPainter.Create(AGraphics, TdxGdiBoxMeasurer(Measurer));
end;

end.
