# Name of the library
NAME			=	AoC15
NAME_1			=	AoC15_1
NAME_2			=	AoC15_2
NAME_1_DEBUG		=	AoC15_debug_1
NAME_2_DEBUG		=	AoC15_debug_2

# Compiler and flags
CC				=	c++
CFLAGS			=	-O0 -Wall -Wextra -Werror -std=c++17
CFLAGS_DEBUG	=	-O0 -Wall -Wextra -Werror -lpthread -g -fsanitize=address
RM				=	rm -f


# Rules
all: $(NAME_1) $(NAME_2)

# Link object files and libft to create the final executable
$(NAME_1):
	$(CC) $(CFLAGS) main_1.cpp -o $(NAME_1)
	@echo "Compiling $(NAME_1) project"

$(NAME_2):
	$(CC) $(CFLAGS) main_2.cpp -o $(NAME_2)
	@echo "Compiling $(NAME_2) project"

debug: fclean
	$(CC) $(CFLAGS_DEBUG) main_1.cpp -o $(NAME_1)
	$(CC) $(CFLAGS_DEBUG) main_2.cpp -o $(NAME_2)
	@echo "Compiling $(NAME) project with debug flags"

# Clean object files
clean:
	@rm -rf $(OBJ_DIR)
	@echo "Deleting $(NAME) objects"

# Full clean: also remove the executable and libft objects
fclean: clean
	@$(RM) $(NAME_1) $(NAME_2)
	@echo "Deleting $(NAME) executables"

# Rebuild everything
re: fclean all

# PHONY prevents conflicts with files named like the targets
.PHONY: all clean fclean re
