unit ResizerCommonTypesUnit;
////////////////////////////////////////////////////////////////////
//                                                                //
// Description: Types, enumerated or otherwise, useful to Resizer //
//              Conversion functions for stated types             //
//                                                                //
////////////////////////////////////////////////////////////////////

interface

type
  TSourceMethod       = (zsmFiles, zsmDirectory, zsmRecursiveDirectory);
  TFiletypeMethod     = (zftmPreserve, zftmConvert);
  TScalingMethod      = (zsmFactor, zsmCalculate);
  TFiletypeConversion = (zftcJPEG, zftcBMP, zftcGIF, zftcPNG);
  TFileHandling       = (zfhOverwrite, zfhSkip);

function StringToSourceMethod(const sValue: String): TSourceMethod;
function StringToFiletypeMethod(const sValue: String): TFiletypeMethod;
function StringToScalingMethod(const sValue: String): TScalingMethod;
function StringToFiletypeConversion(const sValue: String): TFiletypeConversion;
function StringToFileHandling(const sValue: String): TFileHandling;

implementation

uses
  SysUtils, ResizerConstsUnit;

function StringToSourceMethod(const sValue: String): TSourceMethod;
var
  smTemp: TSourceMethod;
  sLowerValue: String;
begin
  smTemp      := zsmFiles;
  sLowerValue := LowerCase(sValue);

  if CompareText(sLowerValue, COMPARE_DIRECTORY) = 0 then
    smTemp    := zsmDirectory
  else
  if CompareText(sLowerValue, COMPARE_RECURSIVE_DIRECTORY) = 0 then
    smTemp    := zsmRecursiveDirectory;

  Result      := smTemp;
end; // StringToSourceMethod

function StringToFiletypeMethod(const sValue: String): TFiletypeMethod;
var
  ftmTemp: TFiletypeMethod;
  sLowerValue: String;
begin
  ftmTemp     := zftmConvert;
  sLowerValue := LowerCase(sValue);

  if CompareText(sLowerValue, COMPARE_PRESERVE) = 0 then
    ftmTemp   := zftmPreserve;

  Result      := ftmTemp;
end; // StringToFiletypeMethod

function StringToScalingMethod(const sValue: String): TScalingMethod;
var
  smTemp: TScalingMethod;
  sLowerValue: String;
begin
  smTemp      := zsmCalculate;
  sLowerValue := LowerCase(sValue);

  if CompareText(sLowerValue, COMPARE_FACTOR) = 0 then
    smTemp    := zsmFactor;

  Result      := smTemp;
end; // StringToScalingMethod

function StringToFiletypeConversion(const sValue: String): TFiletypeConversion;
var
  ftcTemp: TFiletypeConversion;
  sLowerValue: String;
begin
  ftcTemp     := zftcJPEG;
  sLowerValue := LowerCase(sValue);

  if CompareText(sLowerValue, COMPARE_CONV_BMP) = 0 then
    ftcTemp   := zftcBMP;
  //////////////////////////
  // Not implemented yet: //
  //////////////////////////
  {
  else
  if CompareText(sLowerValue, COMPARE_CONV_GIF) = 0 then
    ftcTemp   := zftcGIF
  else
  if CompareText(sLowerValue, COMPARE_CONV_PNG) = 0 then
    ftcTemp   := zftcPNG;
  }

  Result      := ftcTemp;
end; // StringToFiletypeConversion

function StringToFileHandling(const sValue: String): TFileHandling;
var
  fhTemp: TFileHandling;
  sLowerValue: String;
begin
  fhTemp      := zfhSkip;
  sLowerValue := LowerCase(sValue);

  if CompareText(sLowerValue, COMPARE_OVERWRITE) = 0 then
    fhTemp    := zfhOverwrite;

  Result      := fhTemp;
end; // StringToFileHandling

end.

