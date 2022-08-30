{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library graphics classes          }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxShapeReaders;

interface

{$I cxVer.inc}

uses
  dxXMLDoc, dxShapePrimitives, dxCoreClasses;

type
  { TdxShapeFakeBrushShapeReader }

  TdxShapeFakeBrushReader = class(TdxShapeObjectReader)
  protected
    procedure ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode); override;
  end;

  { TdxShapeFakeGradientStopsReader }

  TdxShapeFakeGradientStopsReader = class(TdxShapeObjectReader)
  protected
    procedure ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode); override;
  end;

  { TdxShapeGradientStopReader }

  TdxShapeGradientStopReader = class(TdxShapeObjectReader)
  protected
    procedure ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode); override;
  end;

  { TdxShapeCanvasReader }

  TdxShapeCanvasReader = class(TdxShapeObjectReader)
  protected
    procedure ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode); override;
  end;

  { TdxShapeTransformationReader }

  TdxShapeTransformationReader = class(TdxShapeObjectReader)
  protected
    procedure ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode); override;
  end;

implementation

uses
  dxCustomTree, dxShapeTransformations, dxShapeBrushes;

type
  TdxShapeObjectAccess = class(TdxShapeObject);
  TdxShapeCanvasAccess = class(TdxShapeCanvas);
  TdxShapeTransformationsAccess = class(TdxShapeTransformations);

{ TdxShapeFakeBrushReader }

procedure TdxShapeFakeBrushReader.ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode);
var
  AShape: TdxShapeObject;
  AParent: TdxCustomVectorShape;
begin
  if (ANode <> nil) and ANode.HasChildren then
  begin
    AParent := TdxShapeObjectAccess(AParentShape).Parent as TdxCustomVectorShape;
    AShape := CreateShapeFromNode(AParent, ANode.First);
    AParent.Brush := AShape as TdxShapeCustomBrush;
  end;
end;

{ TdxShapeFakeGradientStopsReader }

procedure TdxShapeFakeGradientStopsReader.ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode);
begin
  inherited ReadChildNodes(TdxShapeObjectAccess(AParentShape).Parent, ANode);
end;

{ TdxShapeGradientStopReader }

procedure TdxShapeGradientStopReader.ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode);
var
  ABrush: TdxShapeCustomGradientBrush;
  AGradientStop: TdxShapeBrushGradientStop;
begin
  ABrush := TdxShapeObjectAccess(AParentShape).Parent as TdxShapeCustomGradientBrush;
  AGradientStop := AParentShape as TdxShapeBrushGradientStop;
  ABrush.GradientPoints.Add(AGradientStop.Offset, dxColorNameToAlphaColor(AGradientStop.Color));
end;

{ TdxShapeCanvasReader }

procedure TdxShapeCanvasReader.ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode);
var
  ACanvasShape: TdxShapeCanvasAccess;
begin
  ACanvasShape := TdxShapeCanvasAccess(AParentShape as TdxShapeCanvas);
  ANode.ForEach(
    procedure (ANode: TdxXMLNode; AUserData: Pointer)
    begin
      ACanvasShape.Add(CreateShapeFromNode(AParentShape, ANode) as TdxCustomVectorShape);
    end);
end;

{ TdxShapeTransformationReader }

procedure TdxShapeTransformationReader.ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode);
var
  AParent: TdxCustomVectorShape;
  ATransformations: TdxShapeTransformationsAccess;
begin
  if (ANode <> nil) and ANode.HasChildren then
  begin
    ANode := ANode.First;
    AParent := TdxShapeObjectAccess(AParentShape).Parent as TdxCustomVectorShape;
    ATransformations := TdxShapeTransformationsAccess(TdxShapeObjectAccess(AParent).Transformations);
    ANode.ForEach(
      procedure (ANode: TdxXMLNode; AUserData: Pointer)
      begin
        ATransformations.AddTransformation(CreateShapeFromNode(AParent, ANode));
      end);
  end;
end;

end.
