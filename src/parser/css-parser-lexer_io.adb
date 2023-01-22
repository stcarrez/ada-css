--  Warning: This file is automatically generated by AFLEX.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.
--  Template: templates/body-io.adb
package body CSS.Parser.Lexer_IO is

   --  gets input and stuffs it into 'buf'.  number of characters read, or YY_NULL,
   --  is returned in 'result'.

   procedure YY_INPUT (buf      : out unbounded_character_array;
                       result   : out Integer;
                       max_size : in Integer) is
      c   : Character;
      i   : Integer := 1;
      loc : Integer := buf'First;
   begin
      if Ada.Text_IO.Is_Open (user_input_file) then
         while i <= max_size loop
            --  Ada ate our newline, put it back on the end.
            if Ada.Text_IO.End_Of_Line (user_input_file) then
               buf (loc) := ASCII.LF;
               Ada.Text_IO.Skip_Line (user_input_file, 1);
            else
               --  UCI CODES CHANGED:
               --    The following codes are modified. Previous codes is commented out.
               --    The purpose of doing this is to make it possible to set Temp_Line
               --    in Ayacc-extension specific codes. Definitely, we can read the character
               --    into the Temp_Line and then set the buf. But Temp_Line will only
               --    be used in Ayacc-extension specific codes which makes
               --    this approach impossible.
               Ada.Text_IO.Get (user_input_file, c);
               buf (loc) := c;
--             Ada.Text_IO.Get (user_input_file, buf (loc));
            end if;

            loc := loc + 1;
            i := i + 1;
         end loop;
      else
         while i <= max_size loop
            if Ada.Text_IO.End_Of_Line then -- Ada ate our newline, put it back on the end.
               buf (loc) := ASCII.LF;
               Ada.Text_IO.Skip_Line (1);

            else
               --  The following codes are modified. Previous codes is commented out.
               --  The purpose of doing this is to make it possible to set Temp_Line
               --  in Ayacc-extension specific codes. Definitely, we can read the character
               --  into the Temp_Line and then set the buf. But Temp_Line will only
               --  be used in Ayacc-extension specific codes which makes this approach impossible.
               Ada.Text_IO.Get (c);
               buf (loc) := c;
               --         get (buf (loc));
            end if;

            loc := loc + 1;
            i := i + 1;
         end loop;
      end if; --  for input file being standard input
      result := i - 1;

   exception
      when Ada.Text_IO.End_Error =>
         result := i - 1;
         --  when we hit EOF we need to set yy_eof_has_been_seen
         yy_eof_has_been_seen := True;

   end YY_INPUT;

   --  yy_get_next_buffer - try to read in new buffer
   --
   --  returns a code representing an action
   --     EOB_ACT_LAST_MATCH -
   --     EOB_ACT_RESTART_SCAN - restart the scanner
   --     EOB_ACT_END_OF_FILE - end of file

   function yy_get_next_buffer return eob_action_type is
      dest           : Integer := 0;
      source         : Integer := yytext_ptr - 1; -- copy prev. char, too
      number_to_move : Integer;
      ret_val        : eob_action_type;
      num_to_read    : Integer;
   begin
      if yy_c_buf_p > yy_n_chars + 1 then
         raise NULL_IN_INPUT;
      end if;

      --  try to read more data

      --  first move last chars to start of buffer
      number_to_move := yy_c_buf_p - yytext_ptr;

      for i in 0 .. number_to_move - 1 loop
         yy_ch_buf (dest) := yy_ch_buf (source);
         dest := dest + 1;
         source := source + 1;
      end loop;

      if yy_eof_has_been_seen then
         --  don't do the read, it's not guaranteed to return an EOF,
         --  just force an EOF

         yy_n_chars := 0;
      else
         num_to_read := YY_BUF_SIZE - number_to_move - 1;

         if num_to_read > YY_READ_BUF_SIZE then
            num_to_read := YY_READ_BUF_SIZE;
         end if;

         --  read in more data
         YY_INPUT (yy_ch_buf (number_to_move .. yy_ch_buf'Last), yy_n_chars, num_to_read);
      end if;
      if yy_n_chars = 0 then
         if number_to_move = 1 then
            ret_val := EOB_ACT_END_OF_FILE;
         else
            ret_val := EOB_ACT_LAST_MATCH;
         end if;

         yy_eof_has_been_seen := True;
      else
         ret_val := EOB_ACT_RESTART_SCAN;
      end if;

      yy_n_chars := yy_n_chars + number_to_move;
      yy_ch_buf (yy_n_chars) := YY_END_OF_BUFFER_CHAR;
      yy_ch_buf (yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

      --  yytext begins at the second character in
      --  yy_ch_buf; the first character is the one which
      --  preceded it before reading in the latest buffer;
      --  it needs to be kept around in case it's a
      --  newline, so yy_get_previous_state() will have
      --  with '^' rules active

      yytext_ptr := 1;

      return ret_val;
   end yy_get_next_buffer;

   function Input return Character is
      c : Character;
   begin
      yy_ch_buf (yy_c_buf_p) := yy_hold_char;

      if yy_ch_buf (yy_c_buf_p) = YY_END_OF_BUFFER_CHAR then
         --  need more input
         yytext_ptr := yy_c_buf_p;
         yy_c_buf_p := yy_c_buf_p + 1;

         case yy_get_next_buffer is
            --  this code, unfortunately, is somewhat redundant with
            --  that above

         when EOB_ACT_END_OF_FILE =>
            if yyWrap then
               yy_c_buf_p := yytext_ptr;
               return ASCII.NUL;
            end if;

            yy_ch_buf (0) := ASCII.LF;
            yy_n_chars := 1;
            yy_ch_buf (yy_n_chars) := YY_END_OF_BUFFER_CHAR;
            yy_ch_buf (yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;
            yy_eof_has_been_seen := False;
            yy_c_buf_p := 1;
            yytext_ptr := yy_c_buf_p;
            yy_hold_char := yy_ch_buf (yy_c_buf_p);

            return Input;
         when EOB_ACT_RESTART_SCAN =>
            yy_c_buf_p := yytext_ptr;

         when EOB_ACT_LAST_MATCH =>
            raise UNEXPECTED_LAST_MATCH;
         end case;
      end if;

      c := yy_ch_buf (yy_c_buf_p);
      yy_c_buf_p := yy_c_buf_p + 1;
      yy_hold_char := yy_ch_buf (yy_c_buf_p);

      return c;
   end Input;

   --  default yywrap function - always treat EOF as an EOF
   function yyWrap return Boolean is
   begin
      return True;
   end yyWrap;

   procedure Open_Input (fname : in String) is
   begin
      yy_init := True;
      Ada.Text_IO.Open (user_input_file, Ada.Text_IO.In_File, fname);
   end Open_Input;

   procedure Close_Input is
   begin
      if Ada.Text_IO.Is_Open (user_input_file) then
         Ada.Text_IO.Close (user_input_file);
      end if;
   end Close_Input;


end CSS.Parser.Lexer_IO;
