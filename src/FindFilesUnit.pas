unit FindFilesUnit;
//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Description: File and directory handling routines                        //
//                                                                          //
//  BuildRecursiveDirList                                                   //
//    Takes a root directory and returns a string list of all that          //
//    directory's subdirectories, recursively                               //
//                                                                          //
//  FindFiles                                                               //
//    Looks for files in the passed in string list with the passed in       //
//    extension and returns the list of files of that extension, full paths //
//    included                                                              //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

interface

uses
  Classes;

function BuildRecursiveDirList(sRoot: String): TStringList;
function FindFiles(slDirs: TStringList; sExtension: String): TStringList;

implementation

uses
  FileCtrl, SysUtils, Forms;

function BuildRecursiveDirList(sRoot: String): TStringList;
//////////////////////////////////////////////////////////////
// Preconditions :                                          //
//  sRoot is a valid directory                              //
//                                                          //
// Output :                                                 //
//  Sorted StringList of subdirectories starting from sRoot //
//  Result StringList must be destroyed by owner            //
//////////////////////////////////////////////////////////////
var
  sDir: String;
  slTemp,
  slResult: TStringList;
  iCheck: integer;
  srSearch: TSearchRec;
begin
  try
    slResult := TStringList.Create;
    try
      slTemp := TStringList.Create;

      ///////////////////////////////////////////////////////////
      // Initialize starting directory for search string lists //
      ///////////////////////////////////////////////////////////
      slTemp.Add(IncludeTrailingBackslash(sRoot));
      slResult.Add(IncludeTrailingBackslash(sRoot));

      while slTemp.Count > 0 do
      begin
        ////////////////////////////////////
        // Start working with a directory //
        ////////////////////////////////////
        sDir := slTemp[0];
        ////////////////////////////////////////////////////////////////
        // "Pop" current scanning directory so we don't scan it again //
        ////////////////////////////////////////////////////////////////
        slTemp.Delete(0);

        ////////////////////////////////////////////////////////
        // Find files - we'll sort for more directories below //
        ////////////////////////////////////////////////////////
        // TODO: can we get away with faDirectory instead of faAnyFile ?
        // iCheck := FindFirst(sDir + '*.*', faAnyFile, srSearch);
        iCheck := FindFirst(sDir + '*.*', faDirectory, srSearch);

        while iCheck = 0 do
        begin
          /////////////////////////////////////////////////////
          // Directory found? Add it to the lists            //
          // slTemp for further recursive directory checking //
          // slResult as a directory that exists             //
          /////////////////////////////////////////////////////
          if ((srSearch.Name <> '.') and (srSearch.Name <> '..'))
            {and ((srSearch.Attr and faDirectory) <> 0)} then
          begin
            slTemp.Add(IncludeTrailingBackslash(sDir + srSearch.Name));
            slResult.Add(IncludeTrailingBackslash(sDir + srSearch.Name));
          end; // if ((srSearch.Name <> '.') and (srSearch.Name <> '..')) and ((srSearch.Attr and faDirectory) <> 0)

          //////////////////////////////////
          // Find the next file/directory //
          //////////////////////////////////
          iCheck := FindNext(srSearch);
                                
          //////////////////////
          // Prevent hang-ups //
          //////////////////////
          Application.ProcessMessages;
        end;
        FindClose(srSearch);
      end; // while slTemp.Count > 0

      if slResult.Count > 0 then
        slResult.Sort;
      Result := slResult;
    finally
      FreeAndNil(slTemp);
    end; // try..finally slTemp.Create
  except on E:Exception do
    Raise Exception('Unable to build subdirectory list starting from ' + sRoot
      + ':' + #10#13 + E.Message);
  end; // try..except
end;

function FindFiles(slDirs: TStringList; sExtension: String): TStringList;
///////////////////////////////////////////////////////////
// Preconditions :                                       //
//  slDirs contains a list of valid directories          //
//  sExtension is a valid file extension, ex: .jpg       //
//                                                       //
// Output :                                              //
//  StringList of file names found in slDirs directories //
//  where extension is the same as sExtension            //  
//  Result StringList must be destroyed by owner         //
///////////////////////////////////////////////////////////
var
  i,
  iCheck: integer;      
  slResult: TStringList;
  srSearch: TSearchRec;
begin
  if slDirs.Count <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  try
    slResult := TStringList.Create;

    for i := 0 to slDirs.Count - 1 do
    begin
      iCheck := FindFirst(slDirs[i] + '*' + sExtension, faAnyFile, srSearch);
      while iCheck = 0 do
      begin
        slResult.Add(slDirs[i] + srSearch.Name);
        iCheck := FindNext(srSearch);
        Application.ProcessMessages;
      end;
      FindClose(srSearch);
    end; // for i := 0 to slDirs.Count - 1

    if slResult.Count > 0 then
      slResult.Sort;

    Result := slResult;
  except on E:Exception do
    Raise Exception.Create('Failure to find ' + sExtension + ' files.' + #10#13 + E.Message);
  end; // try..except
end;

end.

