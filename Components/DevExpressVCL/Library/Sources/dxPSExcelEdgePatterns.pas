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

unit dxPSExcelEdgePatterns;

interface

{$I cxVer.inc}

uses
  Types, Windows, dxCore, dxPSCore, dxPSEdgePatterns;

type
  TdxPSMediumSolidEdgePattern = class(TdxPSSolidEdgePattern)
  public
    class function Name: string; override;
    class function Thickness: Integer; override;
  end;

  TdxPSThickSolidEdgePattern = class(TdxPSSolidEdgePattern)
  public
    class function Name: string; override;
    class function Thickness: Integer; override;
  end;

  TdxPSDottedEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Name: string; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  TdxPSDashedEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Name: string; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  TdxPSDashDotDotEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Name: string; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  TdxPSDashDotEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Name: string; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  TdxPSSlantedDashDotEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Name: string; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  TdxPSMediumDashDotDotEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Name: string; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  TdxPSHairEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Name: string; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  TdxPSMediumDashDotEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Name: string; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  TdxPSMediumDashedEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Name: string; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  TdxPSDoubleLineSolidEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Name: string; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  { TdxPSLongDashedEdgePattern }

  TdxPSLongDashedEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  { TdxPSThickLongDashedEdgePattern }

  TdxPSThickLongDashedEdgePattern = class(TdxPSLongDashedEdgePattern)
  public
    class function Thickness: Integer; override;
  end;

  { TdxPSDoubleWaveEdgePattern }

  TdxPSDoubleWaveEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  { TdxPSHeavyWaveEdgePattern }

  TdxPSHeavyWaveEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

  { TdxPSWaveEdgePattern }

  TdxPSWaveEdgePattern = class(TdxPSEdgePattern)
  public
    class function Bits(Index: Integer): DWORD; override;
    class function Size: TSize; override;
    class function Thickness: Integer; override;
  end;

implementation

uses
  cxClasses, dxPSRes, cxGeometry;

{ TdxPSMediumSolidEdgePattern }

class function TdxPSMediumSolidEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxMediumSolidEdgePattern);
end;

class function TdxPSMediumSolidEdgePattern.Thickness: Integer;
begin
  Result := 2;
end;

{ TdxPSThickSolidEdgePattern }

class function TdxPSThickSolidEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxThickSolidEdgePattern);
end;

class function TdxPSThickSolidEdgePattern.Thickness: Integer;
begin
  Result := 3;
end;

{ TdxPSDottedEdgePattern }

class function TdxPSDottedEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := $33333333;
end;

class function TdxPSDottedEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxSlantedDashDotEdgePattern);
end;

class function TdxPSDottedEdgePattern.Size: TSize;
begin
  Result := cxSize(8, Thickness);
end;

class function TdxPSDottedEdgePattern.Thickness: Integer;
begin
  Result := 1;
end;

{ TdxPSDashedEdgePattern }

class function TdxPSDashedEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := $FF0FF0;
end;

class function TdxPSDashedEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxDashedEdgePattern);
end;

class function TdxPSDashedEdgePattern.Size: TSize;
begin
  Result := cxSize(12, Thickness);
end;

class function TdxPSDashedEdgePattern.Thickness: Integer;
begin
  Result := 1;
end;

{ TdxPSDashDotDotEdgePattern }

class function TdxPSDashDotDotEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := $FF1C7FC7;
end;

class function TdxPSDashDotDotEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxDashDotDotEdgePattern);
end;

class function TdxPSDashDotDotEdgePattern.Size: TSize;
begin
  Result := cxSize(24, Thickness);
end;

class function TdxPSDashDotDotEdgePattern.Thickness: Integer;
begin
  Result := 1;
end;

{ TdxPSDashDotEdgePattern }

class function TdxPSDashDotEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := $FFFC7FC7;
end;

class function TdxPSDashDotEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxDashDotEdgePattern);
end;

class function TdxPSDashDotEdgePattern.Size: TSize;
begin
  Result := cxSize(18, Thickness);
end;

class function TdxPSDashDotEdgePattern.Thickness: Integer;
begin
  Result := 1;
end;

{ TdxPSSlantedDashDotEdgePattern }

class function TdxPSSlantedDashDotEdgePattern.Bits(Index: Integer): DWORD;
begin
  if Ord(Odd(Index)) = 0 then
    Result := $FFFFEFFE
  else
    Result := $FFFCF3FF;
end;

class function TdxPSSlantedDashDotEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxHairEdgePattern);
end;

class function TdxPSSlantedDashDotEdgePattern.Size: TSize;
begin
  Result := cxSize(18, Thickness);
end;

class function TdxPSSlantedDashDotEdgePattern.Thickness: Integer;
begin
  Result := 2;
end;

{ TdxPSMediumDashDotDotEdgePattern }

class function TdxPSMediumDashDotDotEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := $FF1C7FC7;
end;

class function TdxPSMediumDashDotDotEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxMediumDashDotDotEdgePattern);
end;

class function TdxPSMediumDashDotDotEdgePattern.Size: TSize;
begin
  Result := cxSize(24, Thickness);
end;

class function TdxPSMediumDashDotDotEdgePattern.Thickness: Integer;
begin
  Result := 2;
end;

{ TdxPSHairEdgePattern }

class function TdxPSHairEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := $55555555;
end;

class function TdxPSHairEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxDottedEdgePattern);
end;

class function TdxPSHairEdgePattern.Size: TSize;
begin
  Result := cxSize(8, Thickness);
end;

class function TdxPSHairEdgePattern.Thickness: Integer;
begin
  Result := 1;
end;

{ TdxPSMediumDashDotEdgePattern }

class function TdxPSMediumDashDotEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := $FFFC71FF;
end;

class function TdxPSMediumDashDotEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxMediumDashDotEdgePattern);
end;

class function TdxPSMediumDashDotEdgePattern.Size: TSize;
begin
  Result := cxSize(18, Thickness);
end;

class function TdxPSMediumDashDotEdgePattern.Thickness: Integer;
begin
  Result := 2;
end;

{ TdxPSMediumDashedEdgePattern }

class function TdxPSMediumDashedEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := $FFFFF1FF;
end;

class function TdxPSMediumDashedEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxMediumDashedEdgePattern);
end;

class function TdxPSMediumDashedEdgePattern.Size: TSize;
begin
  Result := cxSize(12, Thickness);
end;

class function TdxPSMediumDashedEdgePattern.Thickness: Integer;
begin
  Result := 2;
end;

{ TdxPSDoubleLineSolidEdgePattern }

class function TdxPSDoubleLineSolidEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := $FFFFFFFF * DWORD(Ord(not Odd(Index)));
end;

class function TdxPSDoubleLineSolidEdgePattern.Name: string;
begin
  Result := cxGetResourceString(@sdxDoubleLineEdgePattern);
end;

class function TdxPSDoubleLineSolidEdgePattern.Size: TSize;
begin
  Result := cxSize(8, Thickness);
end;

class function TdxPSDoubleLineSolidEdgePattern.Thickness: Integer;
begin
  Result := 3;
end;

{ TdxPSLongDashedEdgePattern }

class function TdxPSLongDashedEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := $FFFF;
end;

class function TdxPSLongDashedEdgePattern.Size: TSize;
begin
  Result := cxSize(24, Thickness);
end;

class function TdxPSLongDashedEdgePattern.Thickness: Integer;
begin
  Result := 1;
end;

{ TdxPSThickLongDashedEdgePattern }

class function TdxPSThickLongDashedEdgePattern.Thickness: Integer;
begin
  Result := 2;
end;

{ TdxPSDoubleWaveEdgePattern }

class function TdxPSDoubleWaveEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := 0;
  case Index of
    0: Result := $11;
    1: Result := $A;
    2: Result := $15;
    3: Result := $A;
    4: Result := $4;
  end;
end;

class function TdxPSDoubleWaveEdgePattern.Size: TSize;
begin
  Result := cxSize(4, Thickness);
end;

class function TdxPSDoubleWaveEdgePattern.Thickness: Integer;
begin
  Result := 5;
end;

{ TdxPSHeavyWaveEdgePattern }

class function TdxPSHeavyWaveEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := 0;
  case Index of
    0: Result := $11;
    1: Result := $1B;
    2: Result := $FF;
    3: Result := $E;
    4: Result := $4;
  end;
end;

class function TdxPSHeavyWaveEdgePattern.Size: TSize;
begin
  Result := cxSize(4, Thickness);
end;

class function TdxPSHeavyWaveEdgePattern.Thickness: Integer;
begin
  Result := 5;
end;

{ TdxPSWaveEdgePattern }

class function TdxPSWaveEdgePattern.Bits(Index: Integer): DWORD;
begin
  Result := 0;
  case Index of
    0: Result := 2;
    1: Result := 5;
    2: Result := 8;
  end;
end;

class function TdxPSWaveEdgePattern.Size: TSize;
begin
  Result := cxSize(4, 4);
end;

class function TdxPSWaveEdgePattern.Thickness: Integer;
begin
  Result := 3;
end;

procedure RegisterPatterns;
begin
  TdxPSMediumSolidEdgePattern.Register;
  TdxPSThickSolidEdgePattern.Register;
  TdxPSDottedEdgePattern.Register;
  TdxPSDashedEdgePattern.Register;
  TdxPSDashDotDotEdgePattern.Register;
  TdxPSDashDotEdgePattern.Register;
  TdxPSSlantedDashDotEdgePattern.Register;
  TdxPSMediumDashDotDotEdgePattern.Register;
  TdxPSHairEdgePattern.Register;
  TdxPSMediumDashDotEdgePattern.Register;
  TdxPSMediumDashedEdgePattern.Register;
  TdxPSDoubleLineSolidEdgePattern.Register;
  TdxPSDoubleWaveEdgePattern.Register;
  TdxPSHeavyWaveEdgePattern.Register;
  TdxPSWaveEdgePattern.Register;
  TdxPSLongDashedEdgePattern.Register;
  TdxPSThickLongDashedEdgePattern.Register;
end;

procedure UnregisterPatterns;
begin
  TdxPSDoubleLineSolidEdgePattern.Unregister;
  TdxPSMediumDashedEdgePattern.Unregister;
  TdxPSMediumDashDotEdgePattern.Unregister;
  TdxPSHairEdgePattern.Unregister;
  TdxPSMediumDashDotDotEdgePattern.Unregister;
  TdxPSSlantedDashDotEdgePattern.Unregister;
  TdxPSDashDotEdgePattern.Unregister;
  TdxPSDashDotDotEdgePattern.Unregister;
  TdxPSDashedEdgePattern.Unregister;
  TdxPSDottedEdgePattern.Unregister;
  TdxPSThickSolidEdgePattern.Unregister;
  TdxPSMediumSolidEdgePattern.Unregister;
  TdxPSThickLongDashedEdgePattern.Unregister;
  TdxPSDoubleWaveEdgePattern.Unregister;
  TdxPSLongDashedEdgePattern.Unregister;
  TdxPSHeavyWaveEdgePattern.Unregister;
  TdxPSWaveEdgePattern.Unregister;
end;

initialization
  RegisterPatterns;

finalization
  UnregisterPatterns;

end.
