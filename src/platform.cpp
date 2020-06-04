#include "platform.hpp"

#include <unistd.h> // readlink
#include <limits.h> // PATH_MAX

std::filesystem::path plat::get_working_directory(std::error_code &error)
{
        return std::filesystem::current_path(error);
}

void plat::change_directory(const std::filesystem::path &new_dir, std::error_code &error)
{
        std::filesystem::current_path(new_dir, error);
}

std::filesystem::path plat::get_executable_path()
{
        static char path[PATH_MAX];
        static auto len = readlink("/proc/self/exe", path, PATH_MAX);
        static std::filesystem::path ret = std::string(path, PATH_MAX);
        return ret;
}
