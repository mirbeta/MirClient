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

unit dxRichEdit.Utils.WidthsContentInfo;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics;

type
  { TdxWidthsContentInfo }

  TdxWidthsContentInfo = record
  private
    FMinWidth: Integer;
    FMaxWidth: Integer;
  public
    constructor Create(AMinWidth, AMaxWidth: Integer);
    class function Max(const AValue1, AValue2: TdxWidthsContentInfo): TdxWidthsContentInfo; static;
    class function Empty: TdxWidthsContentInfo; static;

    class operator Equal(const AValue1, AValue2: TdxWidthsContentInfo): Boolean;
    class operator NotEqual(const AValue1, AValue2: TdxWidthsContentInfo): Boolean;


    property MinWidth: Integer read FMinWidth;
    property MaxWidth: Integer read FMaxWidth;
  end;

  { TdxWidthsInfo }

  TdxWidthsInfo = record
  private
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FTotalHorizontalMargins: Integer;
  public
    constructor Create(AMinWidth, AMaxWidth, ATotalHorizontalMargins: Integer);
    class function Empty: TdxWidthsInfo; static;

    property MinWidth: Integer read FMinWidth;
    property MaxWidth: Integer read FMaxWidth;
    property TotalHorizontalMargins: Integer read FTotalHorizontalMargins;
  end;

implementation

uses
  Math;

{ TdxWidthsContentInfo }

constructor TdxWidthsContentInfo.Create(AMinWidth, AMaxWidth: Integer);
begin
  FMinWidth := AMinWidth;
  FMaxWidth := AMaxWidth;
end;

class function TdxWidthsContentInfo.Empty: TdxWidthsContentInfo;
begin
  Result.FMinWidth := 0;
  Result.FMaxWidth := 0;
end;

class operator TdxWidthsContentInfo.Equal(const AValue1,
  AValue2: TdxWidthsContentInfo): Boolean;
begin
  Result := CompareMem(@AValue1, @AValue2, SizeOf(TdxWidthsContentInfo));
end;

class function TdxWidthsContentInfo.Max(const AValue1, AValue2: TdxWidthsContentInfo): TdxWidthsContentInfo;
begin
  Result := TdxWidthsContentInfo.Create(Math.Max(AValue1.MinWidth, AValue2.MinWidth), Math.Max(AValue1.MaxWidth, AValue2.MaxWidth));
end;

class operator TdxWidthsContentInfo.NotEqual(const AValue1,
  AValue2: TdxWidthsContentInfo): Boolean;
begin
  Result := not (AValue1 = AValue2);
end;

{ TdxWidthsInfo }

constructor TdxWidthsInfo.Create(AMinWidth, AMaxWidth, ATotalHorizontalMargins: Integer);
begin
  FMinWidth := AMinWidth;
  FMaxWidth := AMaxWidth;
  FTotalHorizontalMargins := ATotalHorizontalMargins;
end;

class function TdxWidthsInfo.Empty: TdxWidthsInfo;
begin
  Result.FMinWidth := 0;
  Result.FMaxWidth := 0;
  Result.FTotalHorizontalMargins := 0;
end;

end.
