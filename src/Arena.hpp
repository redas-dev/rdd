#pragma once
#include <iostream>

class Arena {
public:
    explicit Arena(const size_t bytes): size(bytes)
    {
        mem = static_cast<std::byte*>(malloc(bytes));
        offset = mem;
    }

    template<typename T> inline T* allocate() {
        void* offs = offset;
        offset += sizeof(T);

        return static_cast<T*>(offs);
    }

    Arena(const Arena& other) = delete;

    Arena& operator=(const Arena& other) = delete;

    ~Arena() {
        free(mem);
    }
private:
    size_t size;
    std::byte* mem;
    std::byte* offset;
};