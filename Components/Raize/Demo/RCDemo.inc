{===============================================================================
  RzComps Include File

  This file is included into each unit and serves as a common place to add
  conditional defines and compiler directives to be used by all units.

  Symbol                Meaning
  ------------------------------------------------------------------------------

  DELPHI                Is a Delphi verison of the compiler.
  BCB                   Is a C++Builder version of the compiler.

  VCL120                VCL version 12.0 (Delphi 12.0 - a.k.a. Delphi 2009)
  VCL140                VCL version 14.0 (Delphi 14.0 - a.k.a. Delphi 2010)
  VCL150                VCL version 15.0 (Delphi 15.0 - a.k.a. Delphi XE)
  VCL160                VCL version 16.0 (Delphi 16.0 - a.k.a. Delphi XE2)
  VCL170                VCL version 17.0 (Delphi 17.0 - a.k.a. Delphi XE3)
  VCL180                VCL version 18.0 (Delphi 18.0 - a.k.a. Delphi XE4)
  VCL190                VCL version 19.0 (Delphi 19.0 - a.k.a. Delphi XE5)
  VCL200                VCL version 20.0 (Delphi 20.0 - a.k.a. Delphi XE6)
  VCL210                VCL version 21.0 (Delphi 21.0 - a.k.a. Delphi XE7)
  VCL220                VCL version 22.0 (Delphi 22.0 - a.k.a. Delphi XE8)
  VCL230                VCL version 23.0 (Delphi 23.0 - a.k.a. Delphi XE9)

  VCL120_OR_HIGHER      VCL version 12.0 or higher
  VCL140_OR_HIGHER      VCL version 14.0 or higher
  VCL150_OR_HIGHER      VCL version 15.0 or higher
  VCL160_OR_HIGHER      VCL version 16.0 or higher
  VCL170_OR_HIGHER      VCL version 17.0 or higher
  VCL180_OR_HIGHER      VCL version 18.0 or higher
  VCL190_OR_HIGHER      VCL version 19.0 or higher
  VCL200_OR_HIGHER      VCL version 20.0 or higher
  VCL210_OR_HIGHER      VCL version 21.0 or higher
  VCL220_OR_HIGHER      VCL version 22.0 or higher
  VCL230_OR_HIGHER      VCL version 23.0 or higher

  RS2009                RAD Studio 6
  RS2010                RAD Studio 7
  RSXE                  RAD Studio 8
  RSXE2                 RAD Studio 9
  RSXE3                 RAD Studio 10
  RSXE4                 RAD Studio 11
  RSXE5                 RAD Studio 12
  RSXE6                 RAD Studio 14
  RSXE7                 RAD Studio 15
  RSXE8                 RAD Studio 16
  RSXE9                 RAD Studio 17

  RS2009_OR_HIGHER      RAD Studio 6 or higher
  RS2010_OR_HIGHER      RAD Studio 7 or higher
  RSXE_OR_HIGHER        RAD Studio 8 or higher
  RSXE2_OR_HIGHER       RAD Studio 9 or higher
  RSXE3_OR_HIGHER       RAD Studio 10 or higher
  RSXE4_OR_HIGHER       RAD Studio 11 or higher
  RSXE5_OR_HIGHER       RAD Studio 12 or higher
  RSXE6_OR_HIGHER       RAD Studio 14 or higher
  RSXE7_OR_HIGHER       RAD Studio 15 or higher
  RSXE8_OR_HIGHER       RAD Studio 16 or higher
  RSXE9_OR_HIGHER       RAD Studio 17 or higher

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.
===============================================================================}

{$DEFINE VER_UNKNOWN}

// VER200 (RAD Studio 2009)

{$IFDEF VER200}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE VCL120}
  {$DEFINE RS2009}

  {$DEFINE VCL120_OR_HIGHER}
  
  {$DEFINE RS2009_OR_HIGHER}
{$ENDIF}


// VER210 (RAD Studio 2010)

{$IFDEF VER210}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE VCL140}
  {$DEFINE RS2010}

  {$DEFINE VCL120_OR_HIGHER}
  {$DEFINE VCL140_OR_HIGHER}

  {$DEFINE RS2009_OR_HIGHER}
  {$DEFINE RS2010_OR_HIGHER}
{$ENDIF}


// VER220 (RAD Studio XE)

{$IFDEF VER220}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE VCL150}
  {$DEFINE RSXE}

  {$DEFINE VCL120_OR_HIGHER}
  {$DEFINE VCL140_OR_HIGHER}
  {$DEFINE VCL150_OR_HIGHER}

  {$DEFINE RS2009_OR_HIGHER}
  {$DEFINE RS2010_OR_HIGHER}
  {$DEFINE RSXE_OR_HIGHER}
{$ENDIF}


// VER230 (RAD Studio XE2)

{$IFDEF VER230}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE VCL160}
  {$DEFINE RSXE2}

  {$DEFINE VCL120_OR_HIGHER}
  {$DEFINE VCL140_OR_HIGHER}
  {$DEFINE VCL150_OR_HIGHER}
  {$DEFINE VCL160_OR_HIGHER}

  {$DEFINE RS2009_OR_HIGHER}
  {$DEFINE RS2010_OR_HIGHER}
  {$DEFINE RSXE_OR_HIGHER}
  {$DEFINE RSXE2_OR_HIGHER}
{$ENDIF}


// VER240 (RAD Studio XE3)

{$IFDEF VER240}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE VCL170}
  {$DEFINE RSXE3}

  {$DEFINE VCL120_OR_HIGHER}
  {$DEFINE VCL140_OR_HIGHER}
  {$DEFINE VCL150_OR_HIGHER}
  {$DEFINE VCL160_OR_HIGHER}
  {$DEFINE VCL170_OR_HIGHER}

  {$DEFINE RS2009_OR_HIGHER}
  {$DEFINE RS2010_OR_HIGHER}
  {$DEFINE RSXE_OR_HIGHER}
  {$DEFINE RSXE2_OR_HIGHER}
  {$DEFINE RSXE3_OR_HIGHER}
{$ENDIF}


// VER250 (RAD Studio XE4)

{$IFDEF VER250}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE VCL180}
  {$DEFINE RSXE4}

  {$DEFINE VCL120_OR_HIGHER}
  {$DEFINE VCL140_OR_HIGHER}
  {$DEFINE VCL150_OR_HIGHER}
  {$DEFINE VCL160_OR_HIGHER}
  {$DEFINE VCL170_OR_HIGHER}
  {$DEFINE VCL180_OR_HIGHER}

  {$DEFINE RS2009_OR_HIGHER}
  {$DEFINE RS2010_OR_HIGHER}
  {$DEFINE RSXE_OR_HIGHER}
  {$DEFINE RSXE2_OR_HIGHER}
  {$DEFINE RSXE3_OR_HIGHER}
  {$DEFINE RSXE4_OR_HIGHER}
{$ENDIF}


// VER260 (RAD Studio XE5)

{$IFDEF VER260}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE VCL190}
  {$DEFINE RSXE5}

  {$DEFINE VCL120_OR_HIGHER}
  {$DEFINE VCL140_OR_HIGHER}
  {$DEFINE VCL150_OR_HIGHER}
  {$DEFINE VCL160_OR_HIGHER}
  {$DEFINE VCL170_OR_HIGHER}
  {$DEFINE VCL180_OR_HIGHER}
  {$DEFINE VCL190_OR_HIGHER}

  {$DEFINE RS2009_OR_HIGHER}
  {$DEFINE RS2010_OR_HIGHER}
  {$DEFINE RSXE_OR_HIGHER}
  {$DEFINE RSXE2_OR_HIGHER}
  {$DEFINE RSXE3_OR_HIGHER}
  {$DEFINE RSXE4_OR_HIGHER}
  {$DEFINE RSXE5_OR_HIGHER}
{$ENDIF}

// VER270 (RAD Studio XE6)

{$IFDEF VER270}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE VCL200}
  {$DEFINE RSXE6}

  {$DEFINE VCL120_OR_HIGHER}
  {$DEFINE VCL140_OR_HIGHER}
  {$DEFINE VCL150_OR_HIGHER}
  {$DEFINE VCL160_OR_HIGHER}
  {$DEFINE VCL170_OR_HIGHER}
  {$DEFINE VCL180_OR_HIGHER}
  {$DEFINE VCL190_OR_HIGHER}
  {$DEFINE VCL200_OR_HIGHER}

  {$DEFINE RS2009_OR_HIGHER}
  {$DEFINE RS2010_OR_HIGHER}
  {$DEFINE RSXE_OR_HIGHER}
  {$DEFINE RSXE2_OR_HIGHER}
  {$DEFINE RSXE3_OR_HIGHER}
  {$DEFINE RSXE4_OR_HIGHER}
  {$DEFINE RSXE5_OR_HIGHER}
  {$DEFINE RSXE6_OR_HIGHER}
{$ENDIF}


// VER280 (RAD Studio XE7)

{$IFDEF VER280}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE VCL210}
  {$DEFINE RSXE7}

  {$DEFINE VCL120_OR_HIGHER}
  {$DEFINE VCL140_OR_HIGHER}
  {$DEFINE VCL150_OR_HIGHER}
  {$DEFINE VCL160_OR_HIGHER}
  {$DEFINE VCL170_OR_HIGHER}
  {$DEFINE VCL180_OR_HIGHER}
  {$DEFINE VCL190_OR_HIGHER}
  {$DEFINE VCL200_OR_HIGHER}
  {$DEFINE VCL210_OR_HIGHER}

  {$DEFINE RS2009_OR_HIGHER}
  {$DEFINE RS2010_OR_HIGHER}
  {$DEFINE RSXE_OR_HIGHER}
  {$DEFINE RSXE2_OR_HIGHER}
  {$DEFINE RSXE3_OR_HIGHER}
  {$DEFINE RSXE4_OR_HIGHER}
  {$DEFINE RSXE5_OR_HIGHER}
  {$DEFINE RSXE6_OR_HIGHER}
  {$DEFINE RSXE7_OR_HIGHER}
{$ENDIF}

// VER290 (RAD Studio XE8)

{$IFDEF VER290}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE VCL220}
  {$DEFINE RSXE8}

  {$DEFINE VCL120_OR_HIGHER}
  {$DEFINE VCL140_OR_HIGHER}
  {$DEFINE VCL150_OR_HIGHER}
  {$DEFINE VCL160_OR_HIGHER}
  {$DEFINE VCL170_OR_HIGHER}
  {$DEFINE VCL180_OR_HIGHER}
  {$DEFINE VCL190_OR_HIGHER}
  {$DEFINE VCL200_OR_HIGHER}
  {$DEFINE VCL210_OR_HIGHER}
  {$DEFINE VCL220_OR_HIGHER}

  {$DEFINE RS2009_OR_HIGHER}
  {$DEFINE RS2010_OR_HIGHER}
  {$DEFINE RSXE_OR_HIGHER}
  {$DEFINE RSXE2_OR_HIGHER}
  {$DEFINE RSXE3_OR_HIGHER}
  {$DEFINE RSXE4_OR_HIGHER}
  {$DEFINE RSXE5_OR_HIGHER}
  {$DEFINE RSXE6_OR_HIGHER}
  {$DEFINE RSXE7_OR_HIGHER}
  {$DEFINE RSXE8_OR_HIGHER}
{$ENDIF}

// If compiler version is unknown, assume to be VER300

{$IFDEF VER_UNKNOWN}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE VCL230}
  {$DEFINE RSXE9}

  {$DEFINE VCL120_OR_HIGHER}
  {$DEFINE VCL140_OR_HIGHER}
  {$DEFINE VCL150_OR_HIGHER}
  {$DEFINE VCL160_OR_HIGHER}
  {$DEFINE VCL170_OR_HIGHER}
  {$DEFINE VCL180_OR_HIGHER}
  {$DEFINE VCL190_OR_HIGHER}
  {$DEFINE VCL200_OR_HIGHER}
  {$DEFINE VCL210_OR_HIGHER}
  {$DEFINE VCL220_OR_HIGHER}
  {$DEFINE VCL230_OR_HIGHER}

  {$DEFINE RS2009_OR_HIGHER}
  {$DEFINE RS2010_OR_HIGHER}
  {$DEFINE RSXE_OR_HIGHER}
  {$DEFINE RSXE2_OR_HIGHER}
  {$DEFINE RSXE3_OR_HIGHER}
  {$DEFINE RSXE4_OR_HIGHER}
  {$DEFINE RSXE5_OR_HIGHER}
  {$DEFINE RSXE6_OR_HIGHER}
  {$DEFINE RSXE7_OR_HIGHER}
  {$DEFINE RSXE8_OR_HIGHER}
  {$DEFINE RSXE9_OR_HIGHER}
{$ENDIF}


{$IFDEF BCB}
  {$OBJEXPORTALL ON}
{$ENDIF}


{== Warnings ==================================================================}

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNSUPPORTED_CONSTRUCT OFF}
{WARN UNIT_DEPRECATED OFF}
{WARN SYMBOL_DEPRECATED OFF}


{$IFDEF DOT_NET_WARNINGS_OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}


{$IFDEF CIL}
  {$DEFINE DOT_NET}
  {$DEFINE CLR_11}
  {$IFDEF VER185}
    {$DEFINE CLR_20}
  {$ENDIF}
{$ENDIF}


{== Code Generation Directives ================================================}

{$ALIGN ON}
{$OPTIMIZATION ON}
{$SAFEDIVIDE OFF}
{$STACKFRAMES OFF}

// Runtime Errors

{$IFOPT D+}
  // Compiled with Debug Information
  {$ASSERTIONS ON}
  {$LOCALSYMBOLS ON}
  {$RANGECHECKS ON}
  {$REFERENCEINFO ON}
{$ELSE}
  // Compiled without Debug Information
  {$ASSERTIONS OFF}
  {$LOCALSYMBOLS OFF}
  {$RANGECHECKS OFF}
  {$REFERENCEINFO OFF}
{$ENDIF}

{$IOCHECKS ON}
{$OVERFLOWCHECKS OFF}

// Syntax Options

{$BOOLEVAL OFF}
{$LONGSTRINGS ON}
{$WRITEABLECONST OFF}
{$OPENSTRINGS ON}
{$TYPEDADDRESS OFF}
{$VARSTRINGCHECKS OFF}
{$EXTENDEDSYNTAX ON}

// Miscellaneous Directives

{$IMPORTEDDATA ON}
{$MINENUMSIZE 1}

