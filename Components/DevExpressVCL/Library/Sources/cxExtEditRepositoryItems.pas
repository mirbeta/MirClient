{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxExtEditRepositoryItems;

{$I cxVer.inc}

interface

uses
  Windows, Classes, ComCtrls, Controls, Forms, Graphics, Messages, StdCtrls,
  SysUtils, cxCheckComboBox, cxCheckGroup, cxClasses, cxColorComboBox, cxEdit,
  cxFontNameComboBox, cxLabel, cxProgressBar, cxRichEdit, cxSpinButton,
  cxTextEdit, cxTrackBar, dxColorEdit, dxRatingControl, dxRangeTrackBar;

type
  { TcxEditRepositoryLabel }

  TcxEditRepositoryLabel = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxLabelProperties;
    procedure SetProperties(Value: TcxLabelProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxLabelProperties read GetProperties write SetProperties;
  end;

  { TcxEditRepositoryProgressBar }

  TcxEditRepositoryProgressBar = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxProgressBarProperties;
    procedure SetProperties(Value: TcxProgressBarProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxProgressBarProperties read GetProperties write SetProperties;
  end;

  { TcxEditRepositoryFontComboBox }

  TcxEditRepositoryFontNameComboBox = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxFontNameComboBoxProperties;
    procedure SetProperties(Value: TcxFontNameComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxFontNameComboBoxProperties read GetProperties write SetProperties;
  end;

  { TcxEditRepositoryColorComboBox }

  TcxEditRepositoryColorComboBox = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxColorComboBoxProperties;
    procedure SetProperties(Value: TcxColorComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxColorComboBoxProperties read GetProperties write SetProperties;
  end;

  { TcxEditRepositoryColorEdit }

  TcxEditRepositoryColorEdit = class(TcxEditRepositoryItem)
  private
    function GetProperties: TdxColorEditProperties;
    procedure SetProperties(Value: TdxColorEditProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TdxColorEditProperties read GetProperties write SetProperties;
  end;

  { TcxEditRepositoryTrackBar }

  TcxEditRepositoryTrackBar = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxTrackBarProperties;
    procedure SetProperties(Value: TcxTrackBarProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxTrackBarProperties read GetProperties write SetProperties;
  end;

  { TcxEditRepositoryRangeTrackBar }

  TcxEditRepositoryRangeTrackBar = class(TcxEditRepositoryItem)
  private
    function GetProperties: TdxRangeTrackBarProperties;
    procedure SetProperties(Value: TdxRangeTrackBarProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TdxRangeTrackBarProperties read GetProperties write SetProperties;
  end;

  { TcxEditRepositoryCheckComboBox }

  TcxEditRepositoryCheckComboBox = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxCheckComboBoxProperties;
    procedure SetProperties(Value: TcxCheckComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxCheckComboBoxProperties read GetProperties write SetProperties;
  end;

  { TcxEditRepositoryCheckGroupItem }

  TcxEditRepositoryCheckGroupItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxCheckGroupProperties;
    procedure SetProperties(Value: TcxCheckGroupProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxCheckGroupProperties read GetProperties
      write SetProperties;
  end;

  { TcxEditRepositoryRichItem }

  TcxEditRepositoryRichItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxRichEditProperties;
    procedure SetProperties(Value: TcxRichEditProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxRichEditProperties read GetProperties write SetProperties;
  end;

    { TcxEditRepositoryRatingControl }

  TcxEditRepositoryRatingControl = class(TcxEditRepositoryItem)
  private
    function GetProperties: TdxRatingControlProperties;
    procedure SetProperties(Value: TdxRatingControlProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TdxRatingControlProperties read GetProperties write SetProperties;
  end;

implementation

{ TcxEditRepositoryLabel }

class function TcxEditRepositoryLabel.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxLabelProperties;
end;

function TcxEditRepositoryLabel.GetProperties: TcxLabelProperties;
begin
  Result := inherited Properties as TcxLabelProperties;
end;

procedure TcxEditRepositoryLabel.SetProperties(Value: TcxLabelProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryProgressBar }

class function TcxEditRepositoryProgressBar.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxProgressBarProperties;
end;

function TcxEditRepositoryProgressBar.GetProperties: TcxProgressBarProperties;
begin
  Result := inherited Properties as TcxProgressBarProperties;
end;

procedure TcxEditRepositoryProgressBar.SetProperties(Value: TcxProgressBarProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryFontNameComboBox }

class function TcxEditRepositoryFontNameComboBox.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxFontNameComboBoxProperties;
end;

function TcxEditRepositoryFontNameComboBox.GetProperties: TcxFontNameComboBoxProperties;
begin
  Result := inherited Properties as TcxFontNameComboBoxProperties;
end;

procedure TcxEditRepositoryFontNameComboBox.SetProperties(Value: TcxFontNameComboBoxProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryColorComboBox }

class function TcxEditRepositoryColorComboBox.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxColorComboBoxProperties;
end;

function TcxEditRepositoryColorComboBox.GetProperties: TcxColorComboBoxProperties;
begin
  Result := inherited Properties as TcxColorComboBoxProperties;
end;

procedure TcxEditRepositoryColorComboBox.SetProperties(Value: TcxColorComboBoxProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryColorEdit }

class function TcxEditRepositoryColorEdit.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxColorEditProperties;
end;

function TcxEditRepositoryColorEdit.GetProperties: TdxColorEditProperties;
begin
  Result := inherited Properties as TdxColorEditProperties;
end;

procedure TcxEditRepositoryColorEdit.SetProperties(Value: TdxColorEditProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryTrackBar }

class function TcxEditRepositoryTrackBar.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTrackBarProperties;
end;

function TcxEditRepositoryTrackBar.GetProperties: TcxTrackBarProperties;
begin
  Result := inherited Properties as TcxTrackBarProperties;
end;

procedure TcxEditRepositoryTrackBar.SetProperties(Value: TcxTrackBarProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryRangeTrackBar }

class function TcxEditRepositoryRangeTrackBar.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxRangeTrackBarProperties;
end;

function TcxEditRepositoryRangeTrackBar.GetProperties: TdxRangeTrackBarProperties;
begin
  Result := inherited Properties as TdxRangeTrackBarProperties;
end;

procedure TcxEditRepositoryRangeTrackBar.SetProperties(
  Value: TdxRangeTrackBarProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryCheckComboBox }

class function TcxEditRepositoryCheckComboBox.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckComboBoxProperties;
end;

function TcxEditRepositoryCheckComboBox.GetProperties: TcxCheckComboBoxProperties;
begin
  Result := inherited Properties as TcxCheckComboBoxProperties;
end;

procedure TcxEditRepositoryCheckComboBox.SetProperties(Value: TcxCheckComboBoxProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryCheckGroupItem }

class function TcxEditRepositoryCheckGroupItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckGroupProperties;
end;

function TcxEditRepositoryCheckGroupItem.GetProperties: TcxCheckGroupProperties;
begin
  Result := inherited Properties as TcxCheckGroupProperties;
end;

procedure TcxEditRepositoryCheckGroupItem.SetProperties(
  Value: TcxCheckGroupProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryRichItem }

class function TcxEditRepositoryRichItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxRichEditProperties;
end;

function TcxEditRepositoryRichItem.GetProperties: TcxRichEditProperties;
begin
  Result := inherited Properties as TcxRichEditProperties;
end;

procedure TcxEditRepositoryRichItem.SetProperties(
  Value: TcxRichEditProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryRatingControl }

class function TcxEditRepositoryRatingControl.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxRatingControlProperties;
end;

function TcxEditRepositoryRatingControl.GetProperties: TdxRatingControlProperties;
begin
  Result := inherited Properties as TdxRatingControlProperties;
end;

procedure TcxEditRepositoryRatingControl.SetProperties(Value: TdxRatingControlProperties);
begin
  inherited Properties := Value;
end;

initialization
  RegisterClasses([TcxEditRepositoryLabel, TcxEditRepositoryProgressBar,
    TcxEditRepositoryFontNameComboBox, TcxEditRepositoryColorComboBox,
    TcxEditRepositoryColorEdit,
    TcxEditRepositoryTrackBar, TcxEditRepositoryCheckComboBox,
    TcxEditRepositoryCheckGroupItem, TcxEditRepositoryRichItem, TcxEditRepositoryRatingControl,
    TcxEditRepositoryRangeTrackBar]);

finalization
  UnRegisterClasses([TcxEditRepositoryLabel, TcxEditRepositoryProgressBar,
    TcxEditRepositoryFontNameComboBox, TcxEditRepositoryColorComboBox,
    TcxEditRepositoryColorEdit,
    TcxEditRepositoryTrackBar, TcxEditRepositoryCheckComboBox,
    TcxEditRepositoryCheckGroupItem, TcxEditRepositoryRichItem, TcxEditRepositoryRatingControl,
    TcxEditRepositoryRangeTrackBar]);

end.
