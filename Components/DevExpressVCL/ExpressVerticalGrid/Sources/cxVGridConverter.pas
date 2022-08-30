{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxVGridConverter;

{$I cxVer.inc}

interface

uses
  cxConverterFactory, cxVGrid, cxStyles;

type
  TcxCustomVerticalGridConverterClass = class of TcxCustomVerticalGridConverter;
  TcxStylesAccessor = class(TcxStyles);

  { TcxCustomVerticalGridConverter }

  TcxCustomVerticalGridConverter = class(TcxCustomConverterWithStyles)
  private
    procedure ClearStyles;
    procedure ClearVerticalGrid;
    function GetDestination: TcxCustomVerticalGrid;
  protected
    procedure DoImport; override;
    procedure DoRealImport; virtual;
    procedure SetVerticalGridRowUniqueName(AVerticalGrid: TcxCustomVerticalGrid; ARow: TcxCustomRow);
  public
    property Destination: TcxCustomVerticalGrid read GetDestination;
  end;

const
  cxVGGroupConverterName = 'VerticalGrid Converters';
  cxDBVGGroupConverterName = 'DBVerticalGrid Converters';
  cxRTTIVGGroupConverterName = 'RTTI Inspector Converters';

implementation

{ TcxCustomVerticalGridConverter }

procedure TcxCustomVerticalGridConverter.DoImport;
begin
  Destination.BeginUpdate;
  try
    ClearVerticalGrid;
    DoRealImport;
  finally
    Destination.EndUpdate;
  end;
  inherited DoImport;
end;

procedure TcxCustomVerticalGridConverter.DoRealImport;
begin
end;

procedure TcxCustomVerticalGridConverter.SetVerticalGridRowUniqueName(
  AVerticalGrid: TcxCustomVerticalGrid; ARow: TcxCustomRow);
begin
  //todo
end;

procedure TcxCustomVerticalGridConverter.ClearStyles;
var
  I: Integer;
begin
  for I := 0 to cxvgMaxControlStyleIndex do
    TcxStylesAccessor(Destination.Styles).SetValue(I, nil);
end;

procedure TcxCustomVerticalGridConverter.ClearVerticalGrid;
begin
  with Destination do
  begin
    ClearRows;
    ClearStyles;
    RestoreDefaults;
  end;
end;

function TcxCustomVerticalGridConverter.GetDestination: TcxCustomVerticalGrid;
begin
  Result := inherited Destination as TcxCustomVerticalGrid;
end;

end.
