{===============================================================================
  RzDBNavEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzDBNavigatorImageIndexProperty
    Property editor for TRzDBNavigatorImageIndexes properties.


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzDBNavEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  DesignIntf,
  DesignMenus,
  DesignEditors,
  VCLEditors,
  RzSelectImageEditor,
  ImgList;

type

  {========================================================}
  {== TRzDBNavigatorImageIndexProperty Class Declaration ==}
  {========================================================}

  TRzDBNavigatorImageIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;


implementation

uses
  RzDBNav;

{==============================================}
{== TRzDBNavigatorImageIndexProperty Methods ==}
{==============================================}

function TRzDBNavigatorImageIndexProperty.GetImageList: TCustomImageList;
begin
  Result := ( GetComponent( 0 ) as TRzDBNavigatorImageIndexes ).Navigator.Images;
end;

end.

